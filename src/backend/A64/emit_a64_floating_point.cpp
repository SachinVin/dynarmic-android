/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <optional>
#include <type_traits>
#include <utility>

#include "backend/A64/abi.h"
#include "backend/A64/block_of_code.h"
#include "backend/A64/emit_a64.h"
#include "common/assert.h"
#include "common/common_types.h"
#include "common/fp/fpcr.h"
#include "common/fp/fpsr.h"
#include "common/fp/info.h"
#include "common/fp/op.h"
#include "common/fp/rounding_mode.h"
#include "common/fp/util.h"
#include "frontend/ir/basic_block.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/opcodes.h"

namespace Dynarmic::BackendA64 {

namespace {

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

//void EmitA64::EmitFPAbs16(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    const ARM64Reg result = ctx.reg_alloc.UseScratchXmm(args[0]);
//
//    code.pand(result, code.MConst(xword, f16_non_sign_mask));
//
//    ctx.reg_alloc.DefineValue(inst, result);
//}

void EmitA64::EmitFPAbs32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.UseScratchFpr(args[0]));

    code.fp_emitter.FABS(result, result);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPAbs64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));

    code.fp_emitter.FABS(result, result);

    ctx.reg_alloc.DefineValue(inst, result);
}

//void EmitA64::EmitFPNeg16(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    const ARM64Reg result = ctx.reg_alloc.UseScratchXmm(args[0]);
//
//    code.pxor(result, code.MConst(xword, f16_negative_zero));
//
//    ctx.reg_alloc.DefineValue(inst, result);
//}

void EmitA64::EmitFPNeg32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.UseScratchFpr(args[0]));

    code.fp_emitter.FNEG(result, result);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPNeg64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));

    code.fp_emitter.FNEG(result, result);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPAdd32(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<32, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FADD);
}

void EmitA64::EmitFPAdd64(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<64, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FADD);
}

void EmitA64::EmitFPDiv32(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<32, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FDIV);
}

void EmitA64::EmitFPDiv64(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<64, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FDIV);
}

void EmitA64::EmitFPMul32(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<32, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FMUL);
}

void EmitA64::EmitFPMul64(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<64, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FMUL);
}
void EmitA64::EmitFPSqrt32(EmitContext& ctx, IR::Inst* inst) {
    FPTwoOp<32>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FSQRT);
}

void EmitA64::EmitFPSqrt64(EmitContext& ctx, IR::Inst* inst) {
    FPTwoOp<64>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FSQRT);
}

void EmitA64::EmitFPSub32(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<32, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FSUB);
}

void EmitA64::EmitFPSub64(EmitContext& ctx, IR::Inst* inst) {
    FPThreeOp<64, void(Arm64Gen::ARM64FloatEmitter::*)(ARM64Reg, ARM64Reg, ARM64Reg)>(code, ctx, inst, &Arm64Gen::ARM64FloatEmitter::FSUB);
}

static ARM64Reg SetFpscrNzcvFromFlags(BlockOfCode& code, EmitContext& ctx) {
    ARM64Reg nzcv = ctx.reg_alloc.ScratchGpr();
    // Fpsr's nzcv is copied across integer nzcv 
    code.MRS(nzcv, FIELD_NZCV);
    return nzcv;
}

void EmitA64::EmitFPCompare32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg reg_a = EncodeRegToSingle(ctx.reg_alloc.UseFpr(args[0]));
    ARM64Reg reg_b = EncodeRegToSingle(ctx.reg_alloc.UseFpr(args[1]));
    bool exc_on_qnan = args[2].GetImmediateU1();

    if (exc_on_qnan) {
        code.fp_emitter.FCMPE(reg_a, reg_b);
    } else {
        code.fp_emitter.FCMP(reg_a, reg_b);
    }

    ARM64Reg nzcv = SetFpscrNzcvFromFlags(code, ctx);
    ctx.reg_alloc.DefineValue(inst, nzcv);
}

void EmitA64::EmitFPCompare64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg reg_a = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[0]));
    const ARM64Reg reg_b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));
    bool exc_on_qnan = args[2].GetImmediateU1();

    if (exc_on_qnan) {
        code.fp_emitter.FCMPE(reg_a, reg_b);
    } else {
        code.fp_emitter.FCMP(reg_a, reg_b);
    }

    ARM64Reg nzcv = SetFpscrNzcvFromFlags(code, ctx);
    ctx.reg_alloc.DefineValue(inst, nzcv);
}

void EmitA64::EmitFPHalfToDouble(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.UseScratchFpr(args[0]));

    code.fp_emitter.FCVT(64, 16, result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPHalfToSingle(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.UseScratchFpr(args[0]));
    code.fp_emitter.FCVT(32, 16, result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPSingleToDouble(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.UseScratchFpr(args[0]));

    code.fp_emitter.FCVT(64, 32, result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPSingleToHalf(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.UseScratchFpr(args[0]));
    code.fp_emitter.FCVT(16, 32, result, result);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPDoubleToHalf(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    code.fp_emitter.FCVT(16, 64, result, result);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPDoubleToSingle(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    code.fp_emitter.FCVT(32, 64, result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

template<size_t fsize, bool unsigned_, size_t isize>
static void EmitFPToFixed(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const size_t fbits = args[1].GetImmediateU8();
    const auto rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    const auto round_imm = ConvertRoundingModeToA64RoundingMode(rounding_mode);

    ASSERT_MSG(fbits == 0, "fixed point conversions are not supported yet");

    ARM64Reg src = ctx.reg_alloc.UseScratchFpr(args[0]);
    ARM64Reg result = ctx.reg_alloc.ScratchGpr();
    src = fsize == 64 ? EncodeRegToDouble(src) : EncodeRegToSingle(src);
    result = isize == 64 ? result : DecodeReg(result);

    if constexpr (unsigned_) {
        code.fp_emitter.FCVTU(result, src, round_imm);
    }
    else {
        code.fp_emitter.FCVTS(result, src, round_imm);
    }

    ctx.reg_alloc.DefineValue(inst, result);

}

void EmitA64::EmitFPDoubleToFixedS32(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<64, false, 32>(code, ctx, inst);
}

void EmitA64::EmitFPDoubleToFixedS64(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<64, false, 64>(code, ctx, inst);
}

void EmitA64::EmitFPDoubleToFixedU32(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<64, true, 32>(code, ctx, inst);
}

void EmitA64::EmitFPDoubleToFixedU64(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<64, true, 64>(code, ctx, inst);
}

void EmitA64::EmitFPSingleToFixedS32(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<32, false, 32>(code, ctx, inst);
}

void EmitA64::EmitFPSingleToFixedS64(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<32, false, 64>(code, ctx, inst);
}

void EmitA64::EmitFPSingleToFixedU32(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<32, true, 32>(code, ctx, inst);
}

void EmitA64::EmitFPSingleToFixedU64(EmitContext& ctx, IR::Inst* inst) {
    EmitFPToFixed<32, true, 64>(code, ctx, inst);
}

void EmitA64::EmitFPFixedS32ToSingle(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg from = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.SCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.SCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedU32ToSingle(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    
    const ARM64Reg from = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.UCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.UCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedS32ToDouble(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg from = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.SCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.SCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedS64ToDouble(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg from = ctx.reg_alloc.UseGpr(args[0]);
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.SCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.SCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedS64ToSingle(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg from = ctx.reg_alloc.UseGpr(args[0]);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.SCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.SCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedU32ToDouble(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg from = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.UCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.UCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedU64ToDouble(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);


    const ARM64Reg from = ctx.reg_alloc.UseGpr(args[0]);
    const ARM64Reg result = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.UCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.UCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitFPFixedU64ToSingle(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);


    const ARM64Reg from = ctx.reg_alloc.UseGpr(args[0]);
    const ARM64Reg result = EncodeRegToSingle(ctx.reg_alloc.ScratchFpr());
    const size_t fbits = args[1].GetImmediateU8();
    const FP::RoundingMode rounding_mode = static_cast<FP::RoundingMode>(args[2].GetImmediateU8());
    ASSERT(rounding_mode == ctx.FPSCR_RMode());

    if (fbits != 0) {
        code.fp_emitter.UCVTF(result, from, fbits);
    }
    else {
        code.fp_emitter.UCVTF(result, from);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}
} // namespace Dynarmic::BackendA64
