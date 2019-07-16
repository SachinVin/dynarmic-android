/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include "backend/A64/block_of_code.h"
#include "backend/A64/emit_a64.h"
#include "common/assert.h"
#include "common/common_types.h"
#include "frontend/ir/basic_block.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/opcodes.h"

namespace Dynarmic::BackendA64 {

void EmitA64::EmitPack2x32To1x64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg lo = ctx.reg_alloc.UseScratchGpr(args[0]);
    ARM64Reg hi = ctx.reg_alloc.UseScratchGpr(args[1]);

    // code.MOV(lo, DecodeReg(lo)); // Zero extend to 64-bits
    code.ORR(lo, lo, hi, ArithOption{hi, ST_LSL, 32});

    ctx.reg_alloc.DefineValue(inst, lo);
}

//void EmitA64::EmitPack2x64To1x128(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    Xbyak::Reg64 lo = ctx.reg_alloc.UseGpr(args[0]);
//    Xbyak::Reg64 hi = ctx.reg_alloc.UseGpr(args[1]);
//    Xbyak::Xmm result = ctx.reg_alloc.ScratchXmm();
//
//    if (code.DoesCpuSupport(Xbyak::util::Cpu::tSSE41)) {
//        code.movq(result, lo);
//        code.pinsrq(result, hi, 1);
//    } else {
//        Xbyak::Xmm tmp = ctx.reg_alloc.ScratchXmm();
//        code.movq(result, lo);
//        code.movq(tmp, hi);
//        code.punpcklqdq(result, tmp);
//    }
//
//    ctx.reg_alloc.DefineValue(inst, result);
//}

void EmitA64::EmitLeastSignificantWord(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ctx.reg_alloc.DefineValue(inst, args[0]);
}

void EmitA64::EmitMostSignificantWord(EmitContext& ctx, IR::Inst* inst) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);

    if (carry_inst) {
        ARM64Reg carry = ctx.reg_alloc.ScratchGpr();
        code.UBFX(carry, result, 31, 1);
        ctx.reg_alloc.DefineValue(carry_inst, carry);
        ctx.EraseInstruction(carry_inst);
    } 

    code.LSR(result, result, 32);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitLeastSignificantHalf(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ctx.reg_alloc.DefineValue(inst, args[0]);
}

void EmitA64::EmitLeastSignificantByte(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ctx.reg_alloc.DefineValue(inst, args[0]);
}

void EmitA64::EmitMostSignificantBit(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    // TODO: Flag optimization
    code.LSR(result,result, 31);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitIsZero32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    // TODO: Flag optimization
    code.CMP(result, WZR);
    code.CSET(result, CC_EQ);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitIsZero64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    // TODO: Flag optimization
    code.CMP(result, WZR);
    code.CSET(result, CC_EQ);
    ctx.reg_alloc.DefineValue(inst, result);
}

//void EmitA64::EmitTestBit(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    Xbyak::Reg64 result = ctx.reg_alloc.UseScratchGpr(args[0]);
//    ASSERT(args[1].IsImmediate());
//    // TODO: Flag optimization
//    code.bt(result, args[1].GetImmediateU8());
//    code.setc(result.cvt8());
//    ctx.reg_alloc.DefineValue(inst, result);
//}

static void EmitConditionalSelect(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, int bitsize) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg nzcv = ctx.reg_alloc.ScratchGpr();
    Arm64Gen::ARM64Reg then_ = ctx.reg_alloc.UseGpr(args[1]);
    Arm64Gen::ARM64Reg else_ = ctx.reg_alloc.UseScratchGpr(args[2]);

    then_ = bitsize == 64 ? then_ : DecodeReg(then_);
    else_ = bitsize == 64 ? else_ : DecodeReg(else_);

    code.LDR(INDEX_UNSIGNED, DecodeReg(nzcv), X28, code.GetJitStateInfo().offsetof_CPSR_nzcv);
    // TODO: Flag optimization
    code._MSR(FIELD_NZCV, nzcv);

    switch (args[0].GetImmediateCond()) {
    case IR::Cond::EQ: //z
        code.CSEL(else_, else_, then_ , CC_EQ);
        break;
    case IR::Cond::NE: //!z
        code.CSEL(else_, else_, then_, CC_NEQ);
        break;
    case IR::Cond::CS: //c
        code.CSEL(else_, else_, then_, CC_CS);
        break;
    case IR::Cond::CC: //!c
        code.CSEL(else_, else_, then_ , CC_CC);
        break;
    case IR::Cond::MI: //n
        code.CSEL(else_, else_, then_, CC_MI);
        break;
    case IR::Cond::PL: //!n
        code.CSEL(else_, else_, then_, CC_PL);
        break;
    case IR::Cond::VS: //v
        code.CSEL(else_, else_, then_, CC_VS);
        break;
    case IR::Cond::VC: //!v
        code.CSEL(else_, else_, then_, CC_VC);
        break;
    case IR::Cond::HI: //c & !z
        code.CSEL(else_, else_, then_, CC_HI);
        break;
    case IR::Cond::LS: //!c | z
        code.CSEL(else_, else_, then_, CC_LS);
        break;
    case IR::Cond::GE: // n == v
        code.CSEL(else_, else_, then_, CC_GE);
        break;
    case IR::Cond::LT: // n != v
        code.CSEL(else_, else_, then_, CC_LT);
        break;
    case IR::Cond::GT: // !z & (n == v)
        code.CSEL(else_, else_, then_, CC_GT);
        break;
    case IR::Cond::LE: // z | (n != v)
        code.CSEL(else_, else_, then_, CC_LE);
        break;
    case IR::Cond::AL:
    case IR::Cond::NV:
        code.MOV(else_, then_);
        break;
    default:
        ASSERT_MSG(false, "Invalid cond {}", static_cast<size_t>(args[0].GetImmediateCond()));
    }

    ctx.reg_alloc.DefineValue(inst, else_);
}

void EmitA64::EmitConditionalSelect32(EmitContext& ctx, IR::Inst* inst) {
    EmitConditionalSelect(code, ctx, inst, 32);
}

void EmitA64::EmitConditionalSelect64(EmitContext& ctx, IR::Inst* inst) {
    EmitConditionalSelect(code, ctx, inst, 64);
}

void EmitA64::EmitConditionalSelectNZCV(EmitContext& ctx, IR::Inst* inst) {
    EmitConditionalSelect(code, ctx, inst, 32);
}
} // namespace Dynarmic::BackendA64
