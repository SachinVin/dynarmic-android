/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <limits>

#include "backend/A64/block_of_code.h"
#include "backend/A64/emit_a64.h"
#include "common/assert.h"
#include "common/bit_util.h"
#include "common/common_types.h"
#include "common/mp/integer.h"
#include "frontend/ir/basic_block.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/opcodes.h"

namespace Dynarmic::BackendA64 {

namespace mp = Dynarmic::Common::mp;

void EmitA64::EmitSignedSaturation(EmitContext& ctx, IR::Inst* inst) {
    const auto overflow_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetOverflowFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const size_t N = args[1].GetImmediateU8();
    ASSERT(N >= 1 && N <= 32);

    if (N == 32) {
        if (overflow_inst) {
            const auto no_overflow = IR::Value(false);
            overflow_inst->ReplaceUsesWith(no_overflow);
        }
        ctx.reg_alloc.DefineValue(inst, args[0]);
        return;
    }

    const u32 mask = (1u << N) - 1;
    const u32 positive_saturated_value = (1u << (N - 1)) - 1;
    const u32 negative_saturated_value = 1u << (N - 1);
    const u32 sext_negative_satured_value = Common::SignExtend(N, negative_saturated_value);

    const ARM64Reg result = DecodeReg(ctx.reg_alloc.ScratchGpr());
    const ARM64Reg reg_a = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    const ARM64Reg overflow = DecodeReg(ctx.reg_alloc.ScratchGpr());
    const ARM64Reg tmp = DecodeReg(ctx.reg_alloc.ScratchGpr());

    // overflow now contains a value between 0 and mask if it was originally between {negative,positive}_saturated_value.
    code.ADDI2R(overflow, reg_a, negative_saturated_value, overflow);

    // Put the appropriate saturated value in result
    code.MOVI2R(tmp, positive_saturated_value);
    code.CMP(reg_a, tmp);
    code.MOVI2R(result, sext_negative_satured_value);
    code.CSEL(result, tmp, result, CC_GT);

    // Do the saturation
    code.CMPI2R(overflow, mask, tmp);
    code.CSEL(result, reg_a, result, CC_LS);

    if (overflow_inst) {
        code.CSET(overflow, CC_HI);

        ctx.reg_alloc.DefineValue(overflow_inst, overflow);
        ctx.EraseInstruction(overflow_inst);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitUnsignedSaturation(EmitContext& ctx, IR::Inst* inst) {
    const auto overflow_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetOverflowFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    const size_t N = args[1].GetImmediateU8();
    ASSERT(N <= 31);

    const u32 saturated_value = (1u << N) - 1;

    const ARM64Reg result = DecodeReg(ctx.reg_alloc.ScratchGpr());
    const ARM64Reg reg_a = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    const ARM64Reg overflow = DecodeReg(ctx.reg_alloc.ScratchGpr());

    // Pseudocode: result = clamp(reg_a, 0, saturated_value);
    code.MOVI2R(result, saturated_value);
    code.CMP(reg_a, result);
    code.CSEL(result, WZR, result, CC_LE);
    code.CSEL(result, reg_a, result, CC_LS);

    if (overflow_inst) {
        code.CSET(overflow, CC_HI);

        ctx.reg_alloc.DefineValue(overflow_inst, overflow);
        ctx.EraseInstruction(overflow_inst);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

} // namespace Dynarmic::BackendA64
