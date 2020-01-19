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
    code.CMP(result, ZR);
    code.CSET(result, CC_EQ);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitTestBit(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    ASSERT(args[1].IsImmediate());
    // TODO: Flag optimization
    code.UBFX(result, result, args[1].GetImmediateU8(), 1);
    ctx.reg_alloc.DefineValue(inst, result);
}

static void EmitConditionalSelect(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, int bitsize) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg nzcv = ctx.reg_alloc.ScratchGpr();
    Arm64Gen::ARM64Reg then_ = ctx.reg_alloc.UseGpr(args[1]);
    Arm64Gen::ARM64Reg else_ = ctx.reg_alloc.UseScratchGpr(args[2]);

    then_ = bitsize == 64 ? then_ : DecodeReg(then_);
    else_ = bitsize == 64 ? else_ : DecodeReg(else_);

    code.LDR(INDEX_UNSIGNED, DecodeReg(nzcv), X28, code.GetJitStateInfo().offsetof_cpsr_nzcv);
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

void EmitA64::EmitLogicalShiftLeft32(EmitContext& ctx, IR::Inst* inst) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];
    auto& carry_arg = args[2];

    if (!carry_inst) {
        if (shift_arg.IsImmediate()) {
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            u8 shift = shift_arg.GetImmediateU8();

            if (shift <= 31) {
                code.LSL(result, result, shift);
            } else {
                code.MOV(result, WZR);
            }

            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            //ctx.reg_alloc.Use(shift_arg, HostLoc::X0);
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);

            code.ANDI2R(shift, shift, 0xFF);
            code.LSLV(result, result, shift);            
            code.CMPI2R(shift, 32);
            code.CSEL(result, WZR, DecodeReg(result), CC_GE);
            ctx.reg_alloc.DefineValue(inst, DecodeReg(result));
        }
    } else {
        if (shift_arg.IsImmediate()) {
            u8 shift = shift_arg.GetImmediateU8();
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            if (shift == 0) {
                // There is nothing more to do.
            } else if (shift < 32) {
                code.UBFX(carry, result, 32 - shift, 1);
                code.LSL(result, result, shift);
            } else if (shift > 32) {
                code.MOV(result, WZR);
                code.MOV(carry, WZR);
            } else {
                code.ANDI2R(carry, result, 1);
                code.MOV(result, WZR);                
            }

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            FixupBranch end;

            code.ANDSI2R(shift, shift, 0xFF);
            // if (Rs & 0xFF == 0) goto end;
            end = code.B(CC_EQ);

            code.CMPI2R(shift, 32);
            code.SUBI2R(shift, shift, 1); // Subtract 1 to get the bit that is shiftedout, into the MSB.
            code.LSLV(result, result, shift);
            code.UBFX(carry, result, 31, 1);
            code.LSL(result, result, 1);

            code.CSEL(result, result, WZR, CC_LT);
            code.CSEL(carry, carry, WZR, CC_LE);

            code.SetJumpTarget(end);

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        }
    }
}

void EmitA64::EmitLogicalShiftLeft64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];

    if (shift_arg.IsImmediate()) {
        ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);
        u8 shift = shift_arg.GetImmediateU8();

        if (shift < 64) {
            code.LSL(result, result, shift);
        } else {
            code.MOV(result, ZR);
        }

        ctx.reg_alloc.DefineValue(inst, result);
    } else {
        ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);
        ARM64Reg shift = ctx.reg_alloc.UseGpr(shift_arg);

        code.LSLV(result, result, shift);

        ctx.reg_alloc.DefineValue(inst, result);
    }
}

void EmitA64::EmitLogicalShiftRight32(EmitContext& ctx, IR::Inst* inst) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];
    auto& carry_arg = args[2];

    if (!carry_inst) {
        if (shift_arg.IsImmediate()) {
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            u8 shift = shift_arg.GetImmediateU8();

            if (shift <= 31) {
                code.LSR(result, result, shift);
            } else {
                code.MOVI2R(result, 0);
            }
            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));

            // The 32-bit A64 LSR instruction masks the shift count by 0x1F before performing the shift.
            // ARM differs from the behaviour: It does not mask the count, so shifts above 31 result in zeros.

            code.ANDI2R(shift, shift, 0xFF);
            code.LSRV(result, result, shift);
            code.CMPI2R(shift, 31);
            code.CSEL(result, WZR, result, CC_GT);

            ctx.reg_alloc.DefineValue(inst, result);
        }
    } else {
        if (shift_arg.IsImmediate()) {
            u8 shift = shift_arg.GetImmediateU8();
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            if (shift == 0) {
                // There is nothing more to do.
            } else if (shift < 32) {
                code.LSR(carry, result, shift - 1);
                code.ANDI2R(carry, carry, 1);
                code.LSR(result,result, shift);
            } else if (shift == 32) {
                code.UBFX(carry, result, 31, 1);
                code.MOV(result, WZR);
            } else {
                code.MOV(result, WZR);
                code.MOV(carry, WZR);
            }

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            // TODO: Optimize this.
            FixupBranch end;

            code.ANDSI2R(shift, shift, 0xFF);
            // if (Rs & 0xFF == 0) goto end;
            end = code.B(CC_EQ);

            code.CMPI2R(shift, 32);
            code.SUBI2R(shift, shift, 1); // Subtract 1 to get the bit that is shifted out to the carry.
            code.LSRV(result, result, shift);
            code.ANDI2R(carry, result, 1);
            code.LSR(result, result, 1);

            code.CSEL(result, result, WZR, CC_LT);
            code.CSEL(carry, carry, WZR, CC_LE);
            
            code.SetJumpTarget(end);            

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        }
    }
}

void EmitA64::EmitLogicalShiftRight64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];

    if (shift_arg.IsImmediate()) {
        ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);
        u8 shift = shift_arg.GetImmediateU8();

        if (shift < 64) {
            code.LSR(result, result, shift);
        } else {
            code.MOV(result, ZR);
        }

        ctx.reg_alloc.DefineValue(inst, result);
    } else {
        ARM64Reg shift = ctx.reg_alloc.UseScratchGpr(shift_arg);
        ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);

        code.ANDI2R(shift, shift, 0xFF);
        code.LSRV(result, result, shift);
        code.CMP(shift, 63);
        code.CSEL(result, WZR, result, CC_GT);

        ctx.reg_alloc.DefineValue(inst, result);
    }
}

void EmitA64::EmitArithmeticShiftRight32(EmitContext& ctx, IR::Inst* inst) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];
    auto& carry_arg = args[2];

    if (!carry_inst) {
        if (shift_arg.IsImmediate()) {
            u8 shift = shift_arg.GetImmediateU8();
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));

            code.ASR(result, result, u8(shift < 31 ? shift : 31));

            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            //ctx.reg_alloc.UseScratch(shift_arg, HostLoc::X0);
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg const31 = DecodeReg(ctx.reg_alloc.ScratchGpr());

            // The 32-bit arm64 ASR instruction masks the shift count by 0x1F before performing the shift.
            // ARM differs from the behaviour: It does not mask the count.

            // We note that all shift values above 31 have the same behaviour as 31 does, so we saturate `shift` to 31.
            code.ANDI2R(shift, shift, 0xFF);
            code.MOVI2R(const31, 31);
            code.CMPI2R(shift, u32(31));
            code.CSEL(shift, shift, const31, CC_LE);
            code.ASRV(result, result, shift);

            ctx.reg_alloc.DefineValue(inst, result);
        }
    } else {
        if (shift_arg.IsImmediate()) {
            u8 shift = shift_arg.GetImmediateU8();
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            if (shift == 0) {
                // There is nothing more to do.
            } else if (shift <= 31) {
                code.ASR(result, result, shift - 1);
                code.ANDI2R(carry, result, 1);
                code.ASR(result, result, 1);
            } else {
                code.ASR(result, result, 31);
                code.ANDI2R(carry, result, 1);
            }

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            // TODO: Optimize this.

            FixupBranch end;

            code.ANDSI2R(shift, shift, 0xFF);
            // if (Rs & 0xFF == 0) goto end;
            end = code.B(CC_EQ);
            // else {
            code.MOVI2R(carry, 32);
            code.CMPI2R(shift, u32(31));
            code.CSEL(shift, shift, carry, CC_LE);
            code.SUBI2R(shift, shift, 1);
            code.ASRV(result, result, shift);
            code.ANDI2R(carry, result, 1);
            code.ASR(result, result, 1);
            // }
            
            code.SetJumpTarget(end);            

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        }
    }
}

//void EmitA64::EmitArithmeticShiftRight64(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    auto& operand_arg = args[0];
//    auto& shift_arg = args[1];
//
//    if (shift_arg.IsImmediate()) {
//        u8 shift = shift_arg.GetImmediateU8();
//        Xbyak::Reg64 result = ctx.reg_alloc.UseScratchGpr(operand_arg);
//
//        code.sar(result, u8(shift < 63 ? shift : 63));
//
//        ctx.reg_alloc.DefineValue(inst, result);
//    } else {
//        ctx.reg_alloc.UseScratch(shift_arg, HostLoc::RCX);
//        Xbyak::Reg64 result = ctx.reg_alloc.UseScratchGpr(operand_arg);
//        Xbyak::Reg64 const63 = ctx.reg_alloc.ScratchGpr();
//
//        // The 64-bit x64 SAR instruction masks the shift count by 0x3F before performing the shift.
//        // ARM differs from the behaviour: It does not mask the count.
//
//        // We note that all shift values above 63 have the same behaviour as 63 does, so we saturate `shift` to 63.
//        code.mov(const63, 63);
//        code.movzx(code.ecx, code.cl);
//        code.cmp(code.ecx, u32(63));
//        code.cmovg(code.ecx, const63);
//        code.sar(result, code.cl);
//
//        ctx.reg_alloc.DefineValue(inst, result);
//    }
//}

void EmitA64::EmitRotateRight32(EmitContext& ctx, IR::Inst* inst) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];
    auto& carry_arg = args[2];

    if (!carry_inst) {
        if (shift_arg.IsImmediate()) {
            u8 shift = shift_arg.GetImmediateU8();
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));

            code.ROR(result, result, u8(shift & 0x1F));

            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));

            // aarch64 ROR instruction does (shift & 0x1F) for us.
            code.RORV(result, result, shift);

            ctx.reg_alloc.DefineValue(inst, result);
        }
    } else {
        if (shift_arg.IsImmediate()) {
            u8 shift = shift_arg.GetImmediateU8();
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            if (shift == 0) {
                // There is nothing more to do.
            } else if ((shift & 0x1F) == 0) {
                code.MOV(carry, result, ArithOption{result, ST_LSR, 31});
            } else {
                code.ROR(result, result, (shift & 0x1F) - 1);
                code.ANDI2R(carry, result, 1);
                code.ROR(result, result, 1);
            }

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        } else {
            Arm64Gen::ARM64Reg shift = DecodeReg(ctx.reg_alloc.UseScratchGpr(shift_arg));
            Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(operand_arg));
            Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(carry_arg));

            // TODO: Optimize

            std::vector<FixupBranch> end; 
            FixupBranch zero_1F;

            code.ANDSI2R(shift, shift, u32(0xFF));
            // if (Rs & 0xFF == 0) goto end;
            end.push_back(code.B(CC_EQ));
            code.ANDSI2R(shift, shift, u32(0x1F));
            zero_1F = code.B(CC_EQ);
            // if (Rs & 0x1F != 0) {
            code.SUBI2R(shift, shift, 1);
            code.RORV(result, result, shift);
            code.ANDI2R(carry, result, 1);
            code.ROR(result, result, 1);
            end.push_back(code.B());
            // } else {
            code.SetJumpTarget(zero_1F);
            code.MOV(carry, result, ArithOption{result, ST_LSR, 31});
            // }

            for (FixupBranch e : end) {
                code.SetJumpTarget(e);
            }

            ctx.reg_alloc.DefineValue(carry_inst, carry);
            ctx.EraseInstruction(carry_inst);
            ctx.reg_alloc.DefineValue(inst, result);
        }
    }
}

void EmitA64::EmitRotateRight64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& operand_arg = args[0];
    auto& shift_arg = args[1];

    if (shift_arg.IsImmediate()) {
        u8 shift = shift_arg.GetImmediateU8();
        ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);

        code.ROR(result, result, u8(shift & 0x3F));

        ctx.reg_alloc.DefineValue(inst, result);
    } else {
        ARM64Reg result = ctx.reg_alloc.UseScratchGpr(operand_arg);
        ARM64Reg shift = ctx.reg_alloc.UseGpr(shift_arg);

        code.RORV(result, result, shift);

        ctx.reg_alloc.DefineValue(inst, result);
    }
}

void EmitA64::EmitRotateRightExtended(EmitContext& ctx, IR::Inst* inst) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    Arm64Gen::ARM64Reg carry = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[1]));
    Arm64Gen::ARM64Reg temp = DecodeReg(ctx.reg_alloc.ScratchGpr());

    if (carry_inst) {
        code.MOV(temp, result);
    }

    // Set carry to the LSB and perform ROR.
    code.BFI(result, carry, 0, 1);
    code.ROR(result, result, 1);    
   
    if (carry_inst) {
        code.ANDI2R(carry, temp, 1);

        ctx.reg_alloc.DefineValue(carry_inst, carry);
        ctx.EraseInstruction(carry_inst);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

static Arm64Gen::ARM64Reg DoCarry(RegAlloc& reg_alloc, Argument& carry_in, IR::Inst* carry_out) {
    if (carry_in.IsImmediate()) {
        return carry_out ? reg_alloc.ScratchGpr() : INVALID_REG;
    } else {
        return carry_out ? reg_alloc.UseScratchGpr(carry_in) : reg_alloc.UseGpr(carry_in);
    }
}

static void EmitAdd(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, int bitsize) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);
    auto overflow_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetOverflowFromOp);
    auto nzcv_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetNZCVFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& carry_in = args[2];

    Arm64Gen::ARM64Reg nzcv = nzcv_inst ? ctx.reg_alloc.ScratchGpr() : INVALID_REG;
    Arm64Gen::ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    Arm64Gen::ARM64Reg carry = DecodeReg(DoCarry(ctx.reg_alloc, carry_in, carry_inst));
    Arm64Gen::ARM64Reg overflow = overflow_inst ? ctx.reg_alloc.ScratchGpr() : INVALID_REG;

    result = bitsize == 64 ? result : DecodeReg(result);

    if (args[1].IsImmediate() && args[1].GetType() == IR::Type::U32) {        
        if (carry_in.IsImmediate()) {
            if (carry_in.GetImmediateU1()) {
                Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
                code.CMP(op_arg, op_arg);
                code.ADCS(result, result, op_arg);
            } else {
                u32 op_arg = args[1].GetImmediateU32();
                code.ADDSI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
            }
        } else {
            Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
            code.CMPI2R(carry, 1);
            code.ADCS(result, result, op_arg);
        }
    } else {
        Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
        if (carry_in.IsImmediate()) {
            if (carry_in.GetImmediateU1()) {
                code.CMP(DecodeReg(op_arg), DecodeReg(op_arg));
                code.ADCS(result, result, op_arg);
            } else {
                code.ADDS(result,result, op_arg);
            }
        } else {
            code.CMPI2R(DecodeReg(carry), 1);
            code.ADCS(result, result, op_arg);
        }
    }

    if (nzcv_inst) {
        code.MRS(nzcv, FIELD_NZCV);
        ctx.reg_alloc.DefineValue(nzcv_inst, nzcv);
        ctx.EraseInstruction(nzcv_inst);
    }
    if (carry_inst) {
        code.CSET(carry, CC_CS);
        ctx.reg_alloc.DefineValue(carry_inst, carry);
        ctx.EraseInstruction(carry_inst);
    }
    if (overflow_inst) {
        code.CSET(overflow, CC_VS);
        ctx.reg_alloc.DefineValue(overflow_inst, overflow);
        ctx.EraseInstruction(overflow_inst);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitAdd32(EmitContext& ctx, IR::Inst* inst) {
    EmitAdd(code, ctx, inst, 32);
}

void EmitA64::EmitAdd64(EmitContext& ctx, IR::Inst* inst) {
    EmitAdd(code, ctx, inst, 64);
}

static void EmitSub(BlockOfCode& code, EmitContext& ctx, IR::Inst* inst, int bitsize) {
    auto carry_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetCarryFromOp);
    auto overflow_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetOverflowFromOp);
    auto nzcv_inst = inst->GetAssociatedPseudoOperation(IR::Opcode::GetNZCVFromOp);

    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    auto& carry_in = args[2];

    Arm64Gen::ARM64Reg nzcv = nzcv_inst ? ctx.reg_alloc.ScratchGpr() : INVALID_REG;
    Arm64Gen::ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    Arm64Gen::ARM64Reg carry = DoCarry(ctx.reg_alloc, carry_in, carry_inst);
    Arm64Gen::ARM64Reg overflow = overflow_inst ? ctx.reg_alloc.ScratchGpr() : INVALID_REG;

    // TODO: Consider using LEA.
    // TODO: Optimize CMP case.

    result = bitsize == 64 ? result : DecodeReg(result);

    if (args[1].IsImmediate() && args[1].GetType() == IR::Type::U32) {        
        if (carry_in.IsImmediate()) {
            if (carry_in.GetImmediateU1()) {
                u32 op_arg = args[1].GetImmediateU32();
                code.SUBSI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
            } else {
                Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);

                code.ADDSI2R(op_arg, op_arg, 0); // Clear carry
                code.SBCS(result, result, op_arg);
            }
        } else {
            Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
            code.CMPI2R(carry, 0x1);
            code.SBCS(result, result, op_arg);
        }
    } else {
        Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
        if (carry_in.IsImmediate()) {
            if (carry_in.GetImmediateU1()) {
                code.SUBS(result, result, op_arg);
            } else {
                code.ADDSI2R(DecodeReg(op_arg), DecodeReg(op_arg), 0); // Clear carry
                code.SBCS(result,result, op_arg);
            }
        } else {
            code.CMPI2R(DecodeReg(carry), 0x1);
            code.SBCS(result,result, op_arg);
        }
    }

    if (nzcv_inst) {
        code.MRS(nzcv, FIELD_NZCV);
        ctx.reg_alloc.DefineValue(nzcv_inst, nzcv);
        ctx.EraseInstruction(nzcv_inst);
    }
    if (carry_inst) {
        code.CSET(carry, CC_CS);
        ctx.reg_alloc.DefineValue(carry_inst, carry);
        ctx.EraseInstruction(carry_inst);
    }
    if (overflow_inst) {
        code.CSET(overflow, CC_VS);
        ctx.reg_alloc.DefineValue(overflow_inst, overflow);
        ctx.EraseInstruction(overflow_inst);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSub32(EmitContext& ctx, IR::Inst* inst) {
    EmitSub(code, ctx, inst, 32);
}

void EmitA64::EmitSub64(EmitContext& ctx, IR::Inst* inst) {
    EmitSub(code, ctx, inst, 64);
}

void EmitA64::EmitMul32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    ARM64Reg op_arg = DecodeReg(ctx.reg_alloc.UseGpr(args[1]));
    
    code.MUL(result, result, op_arg);

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitMul64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);

    code.MUL(result, result, op_arg);

    ctx.reg_alloc.DefineValue(inst, result);
}


void EmitA64::EmitUnsignedDiv32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    const ARM64Reg divisor = DecodeReg(ctx.reg_alloc.UseGpr(args[1]));

    code.UDIV(result, result, divisor);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitUnsignedDiv64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    const ARM64Reg divisor = ctx.reg_alloc.UseGpr(args[1]);

    code.UDIV(result, result, divisor);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignedDiv32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    const ARM64Reg divisor = DecodeReg(ctx.reg_alloc.UseGpr(args[1]));

    code.SDIV(result, result, divisor);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignedDiv64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    const ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    const ARM64Reg divisor = ctx.reg_alloc.UseGpr(args[1]);

    code.SDIV(result, result, divisor);
    ctx.reg_alloc.DefineValue(inst, result);
}


void EmitA64::EmitAnd32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));

    if (args[1].IsImmediate()) {
        u32 op_arg = args[1].GetImmediateU32();
        code.ANDI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
    } else {
        Arm64Gen::ARM64Reg op_arg = DecodeReg(ctx.reg_alloc.UseGpr(args[1]));    
        code.AND(result, result, op_arg);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitAnd64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);

    if (args[1].IsImmediate()) {
        u32 op_arg = args[1].GetImmediateU32();
        code.ANDI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
    }
    else {
        Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
        code.AND(result, result, op_arg);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitEor32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));

    if (args[1].IsImmediate()) {
        u32 op_arg = args[1].GetImmediateU32();
        code.EORI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
    } else {
        Arm64Gen::ARM64Reg op_arg = DecodeReg(ctx.reg_alloc.UseGpr(args[1]));
        code.EOR(result, result, op_arg);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitEor64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);

    if (args[1].IsImmediate()) {
        u32 op_arg = args[1].GetImmediateU32();
        code.EORI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
    }
    else {
        Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
        code.EOR(result, result, op_arg);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitOr32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));

    if (args[1].IsImmediate()) {
        u32 op_arg = args[1].GetImmediateU32();
        code.ORRI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
    } else {
        Arm64Gen::ARM64Reg op_arg = DecodeReg(ctx.reg_alloc.UseGpr(args[1]));
        code.ORR(result, result , op_arg);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitOr64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);

    if (args[1].IsImmediate()) {
        u32 op_arg = args[1].GetImmediateU32();
        code.ORRI2R(result, result, op_arg, ctx.reg_alloc.ScratchGpr());
    }
    else {
        Arm64Gen::ARM64Reg op_arg = ctx.reg_alloc.UseGpr(args[1]);
        code.ORR(result, result, op_arg);
    }

    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitNot32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result;
    if (args[0].IsImmediate()) {
        result = DecodeReg(ctx.reg_alloc.ScratchGpr());
        code.MOVI2R(result, u32(~args[0].GetImmediateU32()));
    } else {
        result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
        code.MVN(result, result);
    }
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitNot64(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg result;
    if (args[0].IsImmediate()) {
        result = ctx.reg_alloc.ScratchGpr();
        code.MOVI2R(result, u32(~args[0].GetImmediateU32()));
    }
    else {
        result = ctx.reg_alloc.UseScratchGpr(args[0]);
        code.MVN(result, result);
    }
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignExtendByteToWord(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    code.SXTB(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignExtendHalfToWord(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    code.SXTH(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignExtendByteToLong(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    code.SXTB(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignExtendHalfToLong(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    code.SXTH(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitSignExtendWordToLong(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    code.SXTW(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitZeroExtendByteToWord(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    code.UXTB(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitZeroExtendHalfToWord(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    code.UXTH(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitZeroExtendByteToLong(EmitContext& ctx, IR::Inst* inst) {
    // a64 zeros upper 32 bits on a 32-bit move
    EmitZeroExtendByteToWord(ctx, inst);
}

void EmitA64::EmitZeroExtendHalfToLong(EmitContext& ctx, IR::Inst* inst) {
    // a64 zeros upper 32 bits on a 32-bit move
    EmitZeroExtendHalfToWord(ctx, inst);
}

void EmitA64::EmitZeroExtendWordToLong(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg result = ctx.reg_alloc.UseScratchGpr(args[0]);
    code.MOV(result, DecodeReg(result));
    ctx.reg_alloc.DefineValue(inst, result);
}

//void EmitA64::EmitZeroExtendLongToQuad(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    if (args[0].IsInGpr()) {
//        Xbyak::Reg64 source = ctx.reg_alloc.UseGpr(args[0]);
//        Xbyak::Xmm result = ctx.reg_alloc.ScratchXmm();
//        code.movq(result, source);
//        ctx.reg_alloc.DefineValue(inst, result);
//    } else {
//        Xbyak::Xmm result = ctx.reg_alloc.UseScratchXmm(args[0]);
//        code.movq(result, result);
//        ctx.reg_alloc.DefineValue(inst, result);
//    }
//}

void EmitA64::EmitByteReverseWord(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    code.REV32(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

void EmitA64::EmitByteReverseHalf(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    Arm64Gen::ARM64Reg result = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
    code.REV16(result, result);
    ctx.reg_alloc.DefineValue(inst, result);
}

//void EmitA64::EmitByteReverseDual(EmitContext& ctx, IR::Inst* inst) {
//    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
//    Xbyak::Reg64 result = ctx.reg_alloc.UseScratchGpr(args[0]);
//    code.bswap(result);
//    ctx.reg_alloc.DefineValue(inst, result);
//}

void EmitA64::EmitCountLeadingZeros32(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ARM64Reg source = DecodeReg(ctx.reg_alloc.UseGpr(args[0]));
    ARM64Reg result = DecodeReg(ctx.reg_alloc.ScratchGpr());

    code.CLZ(result, source);
    ctx.reg_alloc.DefineValue(inst, result);    
}

void EmitA64::EmitCountLeadingZeros64(EmitContext& ctx, IR::Inst* inst) {
   auto args = ctx.reg_alloc.GetArgumentInfo(inst);
   ARM64Reg source = ctx.reg_alloc.UseGpr(args[0]);
   ARM64Reg result = ctx.reg_alloc.ScratchGpr();

   code.CLZ(result, source);
   ctx.reg_alloc.DefineValue(inst, result);
}
} // namespace Dynarmic::BackendA64
