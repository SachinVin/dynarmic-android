/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <optional>
#include <type_traits>
#include <utility>

#include "backend/a64/abi.h"
#include "backend/a64/block_of_code.h"
#include "backend/a64/emit_a64.h"
#include "common/assert.h"
#include "common/common_types.h"
#include "common/fp/fpcr.h"
#include "common/fp/fpsr.h"
#include "common/fp/info.h"
#include "common/fp/op.h"
#include "common/fp/rounding_mode.h"
#include "common/fp/util.h"
#include "common/mp/cartesian_product.h"
#include "common/mp/integer.h"
#include "common/mp/list.h"
#include "common/mp/lut.h"
#include "common/mp/to_tuple.h"
#include "common/mp/vlift.h"
#include "common/mp/vllift.h"
#include "frontend/ir/basic_block.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/opcodes.h"

namespace Dynarmic::BackendA64 {

namespace mp = Dynarmic::Common::mp;

namespace {

const ARM64Reg INVALID_REG = ARM64Reg(-1);

constexpr u64 f16_negative_zero = 0x8000;
constexpr u64 f16_non_sign_mask = 0x7fff;

constexpr u64 f32_negative_zero = 0x80000000u;
constexpr u64 f32_nan = 0x7fc00000u;
constexpr u64 f32_non_sign_mask = 0x7fffffffu;
constexpr u64 f32_smallest_normal = 0x00800000u;

constexpr u64 f64_negative_zero = 0x8000000000000000u;
constexpr u64 f64_nan = 0x7ff8000000000000u;
constexpr u64 f64_non_sign_mask = 0x7fffffffffffffffu;
constexpr u64 f64_smallest_normal = 0x0010000000000000u;

constexpr u64 f64_penultimate_positive_denormal = 0x000ffffffffffffeu;
constexpr u64 f64_max_s32 = 0x41dfffffffc00000u; // 2147483647 as a double
constexpr u64 f64_min_u32 = 0x0000000000000000u; // 0 as a double
constexpr u64 f64_max_u32 = 0x41efffffffe00000u; // 4294967295 as a double
constexpr u64 f64_max_s64_lim = 0x43e0000000000000u; // 2^63 as a double (actual maximum unrepresentable)
constexpr u64 f64_min_u64 = 0x0000000000000000u; // 0 as a double
constexpr u64 f64_max_u64_lim = 0x43f0000000000000u; // 2^64 as a double (actual maximum unrepresentable)

//template<size_t fsize, typename T>
//T ChooseOnFsize([[maybe_unused]] T f32, [[maybe_unused]] T f64) {
//    static_assert(fsize == 32 || fsize == 64, "fsize must be either 32 or 64");
//
//    if constexpr (fsize == 32) {
//        return f32;
//    } else {
//        return f64;
//    }
//}
//
//#define FCODE(NAME) (code.*ChooseOnFsize<fsize>(&Xbyak::CodeGenerator::NAME##s, &Xbyak::CodeGenerator::NAME##d))

Arm64Gen::RoundingMode ConvertRoundingModeToA64RoundingMode(FP::RoundingMode rounding_mode) {
    switch (rounding_mode) {
    case FP::RoundingMode::ToNearest_TieEven:
        return RoundingMode::ROUND_N;
    case FP::RoundingMode::TowardsPlusInfinity:
        return RoundingMode::ROUND_P;
    case FP::RoundingMode::TowardsMinusInfinity:
        return RoundingMode::ROUND_M;
    case FP::RoundingMode::TowardsZero:
        return RoundingMode::ROUND_Z;
    case FP::RoundingMode::ToNearest_TieAwayFromZero:
        return RoundingMode::ROUND_A;
    default:
        UNREACHABLE();
    }
}

//template<size_t fsize>
//void DenormalsAreZero(BlockOfCode& code, ARM64Reg xmm_value, ARM64Reg gpr_scratch) {
//    Xbyak::Label end;
//
//    if constexpr (fsize == 32) {
//        code.fp_emitter.FMOV(DecodeReg(gpr_scratch), xmm_value);
//        code.ANDI2R(DecodeReg(gpr_scratch), DecodeReg(gpr_scratch), u32(0x7FFFFFFF));
//        code.SUBI2R(DecodeReg(gpr_scratch), DecodeReg(gpr_scratch), u32(1));
//        code.CMPI2R(DecodeReg(gpr_scratch), DecodeReg(gpr_scratch), u32(0x007FFFFE));
//    } else {
//        auto mask = code.MConst(xword, f64_non_sign_mask);
//        mask.setBit(64);
//        auto penult_denormal = code.MConst(xword, f64_penultimate_positive_denormal);
//        penult_denormal.setBit(64);
//
//        code.movq(gpr_scratch, xmm_value);
//        code.and_(gpr_scratch, mask);
//        code.sub(gpr_scratch, u32(1));
//        code.cmp(gpr_scratch, penult_denormal);
//    }
//
//    // We need to report back whether we've found a denormal on input.
//    // SSE doesn't do this for us when SSE's DAZ is enabled.
//
//    code.ja(end);
//    code.andps(xmm_value, code.MConst(xword, fsize == 32 ? f32_negative_zero : f64_negative_zero));
//    code.mov(dword[r15 + code.GetJitStateInfo().offsetof_FPSCR_IDC], u32(1 << 7));
//    code.L(end);
//}
//
//template<size_t fsize>
//void ZeroIfNaN(BlockOfCode& code, ARM64Reg xmm_value, ARM64Reg xmm_scratch) {
//    code.xorps(xmm_scratch, xmm_scratch);
//    FCODE(cmpords)(xmm_scratch, xmm_value); // true mask when ordered (i.e.: when not an NaN)
//    code.pand(xmm_value, xmm_scratch);
//}

//template<size_t fsize>
//void ForceToDefaultNaN(BlockOfCode& code, ARM64Reg result) {
//        FixupBranch end;
//
//        code.fp_emitter.FCMP(result);
//        end = code.B(CC_VC);
//        code.LDR(result, code.MConst(fsize == 32 ? f32_nan : f64_nan));
//        code.SetJumpTarget(end);
//}

//template<size_t fsize>
//FixupBranch ProcessNaN(BlockOfCode& code, ARM64Reg a, ARM64Reg scratch) {
//    FixupBranch not_nan, end;
//
//    code.fp_emitter.FCMP(a);
//    not_nan = code.B(CC_VC);
//
//    //code.SwitchToFarCode();
//    //code.SetJumpTarget(nan);
//
//    code.EmitPatchLDR(scratch, fsize == 32 ? 0x00400000 : 0x0008'0000'0000'0000);
//    code.fp_emitter.ORR(EncodeRegToDouble(a), a, scratch);
//
//    end = code.B();
//
//    //code.FlushIcache();
//    //code.SwitchToNearCode();
//    code.SetJumpTarget(not_nan);
//    return end;
//}
//
//template<size_t fsize>
//void PostProcessNaN(BlockOfCode& code, Xbyak::Xmm result, Xbyak::Xmm tmp) {
//    if constexpr (fsize == 32) {
//        code.movaps(tmp, result);
//        code.cmpunordps(tmp, tmp);
//        code.pslld(tmp, 31);
//        code.xorps(result, tmp);
//    }
//    else {
//        code.movaps(tmp, result);
//        code.cmpunordpd(tmp, tmp);
//        code.psllq(tmp, 63);
//        code.xorps(result, tmp);
//    }
//}

// This is necessary because x86 and ARM differ in they way they return NaNs from floating point operations
//
// ARM behaviour:
// op1         op2          result
// SNaN        SNaN/QNaN    op1
// QNaN        SNaN         op2
// QNaN        QNaN         op1
// SNaN/QNaN   other        op1
// other       SNaN/QNaN    op2
//
// x86 behaviour:
// op1         op2          result
// SNaN/QNaN   SNaN/QNaN    op1
// SNaN/QNaN   other        op1
// other       SNaN/QNaN    op2
//
// With ARM: SNaNs take priority. With x86: it doesn't matter.
//
// From the above we can see what differs between the architectures is
// the case when op1 == QNaN and op2 == SNaN.
//
// We assume that registers op1 and op2 are read-only. This function also trashes xmm0.
// We allow for the case where op1 and result are the same register. We do not read from op1 once result is written to.
//template<size_t fsize>
//void EmitPostProcessNaNs(BlockOfCode& code, ARM64Reg result, ARM64Reg op1, ARM64Reg op2, Xbyak::Reg64 tmp, Xbyak::Label end) {
//    using FPT = mp::unsigned_integer_of_size<fsize>;
//    constexpr FPT exponent_mask = FP::FPInfo<FPT>::exponent_mask;
//    constexpr FPT mantissa_msb = FP::FPInfo<FPT>::mantissa_msb;
//    constexpr u8 mantissa_msb_bit = static_cast<u8>(FP::FPInfo<FPT>::explicit_mantissa_width - 1);
//
//    // At this point we know that at least one of op1 and op2 is a NaN.
//    // Thus in op1 ^ op2 at least one of the two would have all 1 bits in the exponent.
//    // Keeping in mind xor is commutative, there are only four cases:
//    // SNaN      ^ SNaN/Inf  -> exponent == 0, mantissa_msb == 0
//    // QNaN      ^ QNaN      -> exponent == 0, mantissa_msb == 0
//    // QNaN      ^ SNaN/Inf  -> exponent == 0, mantissa_msb == 1
//    // SNaN/QNaN ^ Otherwise -> exponent != 0, mantissa_msb == ?
//    //
//    // We're only really interested in op1 == QNaN and op2 == SNaN,
//    // so we filter out everything else.
//    //
//    // We do it this way instead of checking that op1 is QNaN because
//    // op1 == QNaN && op2 == QNaN is the most common case. With this method
//    // that case would only require one branch.
//
//    if (code.DoesCpuSupport(Xbyak::util::Cpu::tAVX)) {
//        code.vxorps(xmm0, op1, op2);
//    } else {
//        code.movaps(xmm0, op1);
//        code.xorps(xmm0, op2);
//    }
//
//    constexpr size_t shift = fsize == 32 ? 0 : 48;
//    if constexpr (fsize == 32) {
//        code.movd(tmp.cvt32(), xmm0);
//    } else {
//        // We do this to avoid requiring 64-bit immediates
//        code.pextrw(tmp.cvt32(), xmm0, shift / 16);
//    }
//    code.and_(tmp.cvt32(), static_cast<u32>((exponent_mask | mantissa_msb) >> shift));
//    code.cmp(tmp.cvt32(), static_cast<u32>(mantissa_msb >> shift));
//    code.jne(end, code.T_NEAR);
//
//    // If we're here there are four cases left:
//    // op1 == SNaN && op2 == QNaN
//    // op1 == Inf  && op2 == QNaN
//    // op1 == QNaN && op2 == SNaN <<< The problematic case
//    // op1 == QNaN && op2 == Inf
//
//    if constexpr (fsize == 32) {
//        code.movd(tmp.cvt32(), op2);
//        code.shl(tmp.cvt32(), 32 - mantissa_msb_bit);
//    } else {
//        code.movq(tmp, op2);
//        code.shl(tmp, 64 - mantissa_msb_bit);
//    }
//    // If op2 is a SNaN, CF = 0 and ZF = 0.
//    code.jna(end, code.T_NEAR);
//
//    // Silence the SNaN as required by spec.
//    if (code.DoesCpuSupport(Xbyak::util::Cpu::tAVX)) {
//        code.vorps(result, op2, code.MConst(xword, mantissa_msb));
//    } else {
//        code.movaps(result, op2);
//        code.orps(result, code.MConst(xword, mantissa_msb));
//    }
//    code.jmp(end, code.T_NEAR);
//}

template <size_t fsize, typename Function>
void FPTwoOp(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, Function fn) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    ARM64Reg result = ctx.reg_alloc.UseScratchFpr(args[0]);
    result = fsize == 32 ? EncodeRegToSingle(result) : EncodeRegToDouble(result);
    if constexpr (std::is_member_function_pointer_v<Function>) {
        (code.fp_emitter.*fn)(result, result);
    } else {
        fn(result);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

template <size_t fsize, typename Function>
void FPThreeOp(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, Function fn) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    ARM64Reg result = ctx.reg_alloc.UseScratchFpr(args[0]);
    ARM64Reg operand = ctx.reg_alloc.UseScratchFpr(args[1]);
    result = fsize == 32 ? EncodeRegToSingle(result) : EncodeRegToDouble(result);
    operand = fsize == 32 ? EncodeRegToSingle(operand) : EncodeRegToDouble(operand);


    if constexpr (std::is_member_function_pointer_v<Function>) {
        (code.fp_emitter.*fn)(result, result, operand);
    }
    else {
        fn(result, result, operand);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}
} // anonymous namespace
} // namespace Dynarmic::BackendX64
