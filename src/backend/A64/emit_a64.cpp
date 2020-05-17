/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <unordered_map>
#include <unordered_set>

#include "backend/A64/block_of_code.h"
#include "backend/A64/emit_a64.h"
#include "backend/A64/hostloc.h"
#include "backend/A64/perf_map.h"
#include "common/assert.h"
#include "common/bit_util.h"
#include "common/common_types.h"
#include "common/scope_exit.h"
#include "common/variant_util.h"
#include "frontend/ir/basic_block.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/opcodes.h"

// TODO: Have ARM flags in host flags and not have them use up GPR registers unless necessary.
// TODO: Actually implement that proper instruction selector you've always wanted to sweetheart.

namespace Dynarmic::BackendA64 {

EmitContext::EmitContext(RegAlloc& reg_alloc, IR::Block& block)
    : reg_alloc(reg_alloc), block(block) {}

void EmitContext::EraseInstruction(IR::Inst* inst) {
    block.Instructions().erase(inst);
    inst->ClearArgs();
}

EmitA64::EmitA64(BlockOfCode& code)
    : code(code) {}

EmitA64::~EmitA64() = default;

std::optional<typename EmitA64::BlockDescriptor> EmitA64::GetBasicBlock(IR::LocationDescriptor descriptor) const {
    auto iter = block_descriptors.find(descriptor);
    if (iter == block_descriptors.end())
        return std::nullopt;
    return iter->second;
}

void EmitA64::EmitVoid(EmitContext&, IR::Inst*) {
}

void EmitA64::EmitBreakpoint(EmitContext&, IR::Inst*) {
    code.BRK(0);
}

void EmitA64::EmitIdentity(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    if (!args[0].IsImmediate()) {
        ctx.reg_alloc.DefineValue(inst, args[0]);
    }
}

void EmitA64::PushRSBHelper(ARM64Reg loc_desc_reg, ARM64Reg index_reg, IR::LocationDescriptor target) {
    auto iter = block_descriptors.find(target);
    CodePtr target_code_ptr = iter != block_descriptors.end()
                            ? iter->second.entrypoint
                            : code.GetReturnFromRunCodeAddress();

    code.LDR(INDEX_UNSIGNED, DecodeReg(index_reg), X28, code.GetJitStateInfo().offsetof_rsb_ptr);

    code.MOVI2R(loc_desc_reg, target.Value());

    patch_information[target].mov_x0.emplace_back(code.GetCodePtr());
    EmitPatchMovX0(target_code_ptr);

    code.ADD(code.ABI_SCRATCH1, X28, DecodeReg(index_reg), ArithOption{index_reg, ST_LSL, 3});
    code.STR(INDEX_UNSIGNED, loc_desc_reg, code.ABI_SCRATCH1, code.GetJitStateInfo().offsetof_rsb_location_descriptors);
    code.STR(INDEX_UNSIGNED, X0, code.ABI_SCRATCH1, code.GetJitStateInfo().offsetof_rsb_codeptrs);

    code.ADDI2R(DecodeReg(index_reg), DecodeReg(index_reg), 1);
    code.ANDI2R(DecodeReg(index_reg), DecodeReg(index_reg), code.GetJitStateInfo().rsb_ptr_mask, code.ABI_SCRATCH1);
    code.STR(INDEX_UNSIGNED, DecodeReg(index_reg), X28, code.GetJitStateInfo().offsetof_rsb_ptr);        
}

void EmitA64::EmitPushRSB(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);
    ASSERT(args[0].IsImmediate());
    u64 unique_hash_of_target = args[0].GetImmediateU64();

    ctx.reg_alloc.ScratchGpr({HostLoc::X0});
    Arm64Gen::ARM64Reg loc_desc_reg = ctx.reg_alloc.ScratchGpr();
    Arm64Gen::ARM64Reg index_reg = ctx.reg_alloc.ScratchGpr();

    PushRSBHelper(loc_desc_reg, index_reg, IR::LocationDescriptor{unique_hash_of_target});
}

void EmitA64::EmitGetCarryFromOp(EmitContext&, IR::Inst*) {
    ASSERT_FALSE("should never happen");
}

void EmitA64::EmitGetOverflowFromOp(EmitContext&, IR::Inst*) {
    ASSERT_FALSE("should never happen");
}

void EmitA64::EmitGetGEFromOp(EmitContext&, IR::Inst*) {
    ASSERT_FALSE("should never happen");
}

void EmitA64::EmitGetUpperFromOp(EmitContext&, IR::Inst*) {
    ASSERT_FALSE("should never happen");
}

void EmitA64::EmitGetLowerFromOp(EmitContext&, IR::Inst*) {
    ASSERT_FALSE("should never happen");
}

void EmitA64::EmitGetNZCVFromOp(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    Arm64Gen::ARM64Reg nzcv = ctx.reg_alloc.ScratchGpr();
    Arm64Gen::ARM64Reg value = ctx.reg_alloc.UseGpr(args[0]);
    code.CMP(value, ZR);
    code.MRS(nzcv, FIELD_NZCV);
    ctx.reg_alloc.DefineValue(inst, nzcv);
}

void EmitA64::EmitNZCVFromPackedFlags(EmitContext& ctx, IR::Inst* inst) {
    auto args = ctx.reg_alloc.GetArgumentInfo(inst);

    if (args[0].IsImmediate()) {
        Arm64Gen::ARM64Reg nzcv = DecodeReg(ctx.reg_alloc.ScratchGpr());
        u32 value = 0;
        value |= Common::Bit<31>(args[0].GetImmediateU32()) ? (1 << 15) : 0;
        value |= Common::Bit<30>(args[0].GetImmediateU32()) ? (1 << 14) : 0;
        value |= Common::Bit<29>(args[0].GetImmediateU32()) ? (1 << 8) : 0;
        value |= Common::Bit<28>(args[0].GetImmediateU32()) ? (1 << 0) : 0;
        code.MOVI2R(nzcv, value);
        ctx.reg_alloc.DefineValue(inst, nzcv);
    } else {
        Arm64Gen::ARM64Reg nzcv = DecodeReg(ctx.reg_alloc.UseScratchGpr(args[0]));
        Arm64Gen::ARM64Reg scratch = DecodeReg(ctx.reg_alloc.ScratchGpr());
        // TODO: Optimize
        code.LSR(nzcv, nzcv, 28);
        code.MOVI2R(scratch, 0b00010000'10000001);
        code.MUL(nzcv, nzcv, scratch);
        code.ANDI2R(nzcv, nzcv, 1, scratch);
        ctx.reg_alloc.DefineValue(inst, nzcv);
    }
}

void EmitA64::EmitAddCycles(size_t cycles) {
    ASSERT(cycles < std::numeric_limits<u32>::max());
    code.SUBI2R(X26, X26, static_cast<u32>(cycles));
}

FixupBranch EmitA64::EmitCond(IR::Cond cond) {
    FixupBranch label;

    const Arm64Gen::ARM64Reg cpsr = code.ABI_SCRATCH1;
    code.LDR(INDEX_UNSIGNED, DecodeReg(cpsr), X28, code.GetJitStateInfo().offsetof_cpsr_nzcv);
    code._MSR(FIELD_NZCV, cpsr);

    switch (cond) {
    case IR::Cond::EQ: //z
        label = code.B(CC_EQ);
        break;
    case IR::Cond::NE: //!z
        label = code.B(CC_NEQ);
        break;
    case IR::Cond::CS: //c
        label = code.B(CC_CS);
        break;
    case IR::Cond::CC: //!c
        label = code.B(CC_CC);
        break;
    case IR::Cond::MI: //n
        label = code.B(CC_MI);
        break;
    case IR::Cond::PL: //!n
        label = code.B(CC_PL);
        break;
    case IR::Cond::VS: //v
        label = code.B(CC_VS);
        break;
    case IR::Cond::VC: //!v
        label = code.B(CC_VC);
        break;
    case IR::Cond::HI:  //c & !z
        label = code.B(CC_HI);
        break;
    case IR::Cond::LS:  //!c | z
        label = code.B(CC_LS);
        break;
    case IR::Cond::GE:  // n == v
        label = code.B(CC_GE);
        break;
    case IR::Cond::LT:  // n != v
        label = code.B(CC_LT);
        break;
    case IR::Cond::GT:  // !z & (n == v)
        label = code.B(CC_GT);
        break;
    case IR::Cond::LE:  // z | (n != v)
        label = code.B(CC_LE);
        break;    
    default:
        ASSERT_MSG(false, "Unknown cond {}", static_cast<size_t>(cond));
        break;
    }

    return label;
}

EmitA64::BlockDescriptor EmitA64::RegisterBlock(const IR::LocationDescriptor& descriptor, CodePtr entrypoint, size_t size) {
    PerfMapRegister(entrypoint, code.GetCodePtr(), LocationDescriptorToFriendlyName(descriptor));
    Patch(descriptor, entrypoint);
    BlockDescriptor block_desc{entrypoint, size};

    block_descriptors.emplace(descriptor.Value(), block_desc);
    return block_desc;
}

void EmitA64::EmitTerminal(IR::Terminal terminal, IR::LocationDescriptor initial_location, bool is_single_step) {
    Common::VisitVariant<void>(terminal, [this, initial_location, is_single_step](auto x) {
        using T = std::decay_t<decltype(x)>;
        if constexpr (!std::is_same_v<T, IR::Term::Invalid>) {
            this->EmitTerminalImpl(x, initial_location, is_single_step);
        } else {
            ASSERT_MSG(false, "Invalid terminal");
        }
    });
}

void EmitA64::Patch(const IR::LocationDescriptor& desc, CodePtr bb) {
    const CodePtr save_code_ptr = code.GetCodePtr();
    const PatchInformation& patch_info = patch_information[desc];

    for (CodePtr location : patch_info.jg) {
        code.SetCodePtr(location);
        EmitPatchJg(desc, bb);
        code.FlushIcache();
    }

    for (CodePtr location : patch_info.jmp) {
        code.SetCodePtr(location);
        EmitPatchJmp(desc, bb);
        code.FlushIcache();
    }

    for (CodePtr location : patch_info.mov_x0) {
        code.SetCodePtr(location);
        EmitPatchMovX0(bb);
        code.FlushIcache();
    }

    code.SetCodePtr(save_code_ptr);
}

void EmitA64::Unpatch(const IR::LocationDescriptor& desc) {
    Patch(desc, nullptr);
}

void EmitA64::ClearCache() {
    block_descriptors.clear();
    patch_information.clear();

    PerfMapClear();
}

void EmitA64::InvalidateBasicBlocks(const std::unordered_set<IR::LocationDescriptor>& locations) {
    code.EnableWriting();
    SCOPE_EXIT { code.DisableWriting(); };

    for (const auto &descriptor : locations) {
        auto it = block_descriptors.find(descriptor);
        if (it == block_descriptors.end()) {
            continue;
        }

        if (patch_information.count(descriptor)) {
            Unpatch(descriptor);
        }
        block_descriptors.erase(it);
    }
}

} // namespace Dynarmic::BackendA64
