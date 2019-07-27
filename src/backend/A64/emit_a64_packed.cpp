/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include "backend/A64/block_of_code.h"
#include "backend/A64/emit_a64.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/opcodes.h"

namespace Dynarmic::BackendA64 {

void EmitA64::EmitPackedAddU8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg sum = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.ADD(B, sum, sum, b);

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.CMHI(B, ge, b, sum);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    ctx.reg_alloc.DefineValue(inst, sum);
}

void EmitA64::EmitPackedAddS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.SQADD(B, ge, a, b);
        code.fp_emitter.CMGE_zero(B, ge, ge);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    code.fp_emitter.ADD(B, a, a, b);

    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedAddU16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg sum = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.ADD(H, sum, sum, b);

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.CMHI(H, ge, b, sum);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    ctx.reg_alloc.DefineValue(inst, sum);
}

void EmitA64::EmitPackedAddS16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.SQADD(H, ge, a, b);
        code.fp_emitter.CMGE_zero(H, ge, ge);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    code.fp_emitter.ADD(H, a, a, b);

    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSubU8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.CMHS(B, ge, a, b);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    code.fp_emitter.SUB(B, a, a, b);

    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSubS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.SQSUB(B, ge, a, b);
        code.fp_emitter.CMGE_zero(B, ge, ge);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    code.fp_emitter.SUB(B, a, a, b);

    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSubU16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.CMHS(H, ge, a, b);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    code.fp_emitter.SUB(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSubS16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    if (ge_inst) {
        const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.ScratchFpr());

        code.fp_emitter.SQSUB(H, ge, a, b);
        code.fp_emitter.CMGE_zero(H, ge, ge);

        ctx.reg_alloc.DefineValue(ge_inst, ge);
        ctx.EraseInstruction(ge_inst);
    }

    code.fp_emitter.SUB(H, a, a, b);

    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingAddU8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UHADD(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingAddU16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UHADD(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingAddS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SHADD(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingAddS16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SHADD(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingSubU8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UHSUB(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingSubS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SHSUB(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingSubU16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UHSUB(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedHalvingSubS16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SHSUB(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitPackedSubAdd(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, bool hi_is_sum, bool is_signed, bool is_halving) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const auto ge_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetGEFromOp);

    const ARM64Reg reg_a_hi = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    const ARM64Reg reg_b_hi = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[1]));
    const ARM64Reg reg_a_lo = DecodeReg(ctx.reg_alloc.ScratchGpr());
    const ARM64Reg reg_b_lo = DecodeReg(ctx.reg_alloc.ScratchGpr());
    ARM64Reg reg_sum, reg_diff;

    if (is_signed) {
        code.SXTH(reg_a_lo, reg_a_hi);
        code.SXTH(reg_b_lo, reg_b_hi);
        code.ASR(reg_a_hi, reg_a_hi, 16);
        code.ASR(reg_b_hi, reg_b_hi, 16);
    } else {
        code.UXTH(reg_a_lo, reg_a_hi);
        code.UXTH(reg_b_lo, reg_b_hi);
        code.LSR(reg_a_hi, reg_a_hi, 16);
        code.LSR(reg_b_hi, reg_b_hi, 16);
    }

    if (hi_is_sum) {
        code.SUB(reg_a_lo, reg_a_lo, reg_b_hi);
        code.ADD(reg_a_hi, reg_a_hi, reg_b_lo);
        reg_diff = reg_a_lo;
        reg_sum = reg_a_hi;
    } else {
        code.ADD(reg_a_lo, reg_a_lo, reg_b_hi);
        code.SUB(reg_a_hi, reg_a_hi, reg_b_lo);
        reg_diff = reg_a_hi;
        reg_sum = reg_a_lo;
    }

    if (ge_inst) {
        // The reg_b registers are no longer required.
        const ARM64Reg ge_sum = reg_b_hi;
        const ARM64Reg ge_diff = reg_b_lo;

        if (!is_signed) {
            code.LSL(ge_sum, reg_sum, 15);
            code.ASR(ge_sum, ge_sum, 31);
        } else {
            code.MVN(ge_sum, reg_sum);
            code.ASR(ge_sum, ge_sum, 31);
        }
        code.MVN(ge_diff, reg_diff);
        code.ASR(ge_diff, ge_diff, 31);
        code.ANDI2R(ge_sum, ge_sum, hi_is_sum ? 0xFFFF0000 : 0x0000FFFF);
        code.ANDI2R(ge_diff, ge_diff, hi_is_sum ? 0x0000FFFF : 0xFFFF0000);
        code.ORR(ge_sum, ge_sum, ge_diff);

        ctx.reg_alloc.DefineValue(ge_inst, ge_sum);
        ctx.EraseInstruction(ge_inst);
    }

    if (is_halving) {
        code.LSR(reg_a_hi, reg_a_hi, 1);
        code.LSR(reg_a_lo, reg_a_lo, 1);
    }

    // reg_a_lo now contains the low word and reg_a_hi now contains the high word.
    // Merge them.
    code.BFM(reg_a_lo, reg_a_hi, 16, 15);

    ctx.reg_alloc.DefineValue(inst, reg_a_lo);
}

void EmitA64::EmitPackedAddSubU16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, true, false, false);
}

void EmitA64::EmitPackedAddSubS16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, true, true, false);
}

void EmitA64::EmitPackedSubAddU16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, false, false, false);
}

void EmitA64::EmitPackedSubAddS16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, false, true, false);
}

void EmitA64::EmitPackedHalvingAddSubU16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, true, false, true);
}

void EmitA64::EmitPackedHalvingAddSubS16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, true, true, true);
}

void EmitA64::EmitPackedHalvingSubAddU16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, false, false, true);
}

void EmitA64::EmitPackedHalvingSubAddS16(EmitContext& ctx, IR::Inst* inst) {
    EmitPackedSubAdd(code, ctx, inst, false, true, true);
}

void EmitA64::EmitPackedSaturatedAddU8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UQADD(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedAddS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SQADD(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedSubU8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UQSUB(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedSubS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SQSUB(B, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedAddU16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UQADD(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedAddS16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SQADD(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedSubU16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UQSUB(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSaturatedSubS16(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.SQSUB(H, a, a, b);
    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedAbsDiffSumS8(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));

    code.fp_emitter.UABD(B, a, a, b);
    code.fp_emitter.UADDLV(B, a, a);

    ctx.reg_alloc.DefineValue(inst, a);
}

void EmitA64::EmitPackedSelect(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg ge = EncodeRegToDouble(ctx.reg_alloc.UseScratchFpr(args[0]));
    const ARM64Reg a = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[1]));
    const ARM64Reg b = EncodeRegToDouble(ctx.reg_alloc.UseFpr(args[2]));

    code.fp_emitter.BSL(ge, b, a);

    ctx.reg_alloc.DefineValue(inst, ge);
}

} // namespace Dynarmic::BackendA64
