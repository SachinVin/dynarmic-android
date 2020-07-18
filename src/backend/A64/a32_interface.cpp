/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <memory>

#include <boost/icl/interval_set.hpp>
#include <fmt/format.h>

#include <dynarmic/A32/a32.h>
#include <dynarmic/A32/context.h>

#include "backend/A64/a32_emit_a64.h"
#include "backend/A64/a32_jitstate.h"
#include "backend/A64/block_of_code.h"
#include "backend/A64/callback.h"
#include "backend/A64/devirtualize.h"
#include "backend/A64/jitstate_info.h"
#include "common/assert.h"
#include "common/common_types.h"
#include "common/llvm_disassemble.h"
#include "common/scope_exit.h"
#include "frontend/A32/translate/translate.h"
#include "frontend/ir/basic_block.h"
#include "frontend/ir/location_descriptor.h"
#include "ir_opt/passes.h"

namespace Dynarmic::A32 {

using namespace BackendA64;

static RunCodeCallbacks GenRunCodeCallbacks(const A32::UserConfig& config, CodePtr (*LookupBlock)(void* lookup_block_arg), void* arg) {
    return RunCodeCallbacks{
        std::make_unique<ArgCallback>(LookupBlock, reinterpret_cast<u64>(arg)),
        std::make_unique<ArgCallback>(Devirtualize<&A32::UserCallbacks::AddTicks>(config.callbacks)),
        std::make_unique<ArgCallback>(Devirtualize<&A32::UserCallbacks::GetTicksRemaining>(config.callbacks)),
        reinterpret_cast<u64>(config.fastmem_pointer),
    };
}

struct Jit::Impl {
    Impl(Jit* jit, A32::UserConfig config)
            : block_of_code(GenRunCodeCallbacks(config, &GetCurrentBlock, this), JitStateInfo{jit_state})
            , emitter(block_of_code, config, jit)
            , config(std::move(config))
            , jit_interface(jit)
    {}

    A32JitState jit_state;
    BlockOfCode block_of_code;
    A32EmitA64 emitter;

    const A32::UserConfig config;

    // Requests made during execution to invalidate the cache are queued up here.
    size_t invalid_cache_generation = 0;
    boost::icl::interval_set<u32> invalid_cache_ranges;
    bool invalidate_entire_cache = false;

    void Execute() {
        const CodePtr current_codeptr = [this]{
            // RSB optimization
            const u32 new_rsb_ptr = (jit_state.rsb_ptr - 1) & A32JitState::RSBPtrMask;
            if (jit_state.GetUniqueHash() == jit_state.rsb_location_descriptors[new_rsb_ptr]) {
                jit_state.rsb_ptr = new_rsb_ptr;
                return reinterpret_cast<CodePtr>(jit_state.rsb_codeptrs[new_rsb_ptr]);
            }

            return GetCurrentBlock();
        }();

        block_of_code.RunCode(&jit_state, current_codeptr);
    }

    std::string Disassemble(const IR::LocationDescriptor& descriptor) {
        auto block = GetBasicBlock(descriptor);
        std::string result = fmt::format("address: {}\nsize: {} bytes\n", block.entrypoint, block.size);
#ifdef DYNARMIC_USE_LLVM
        for (const u32* pos = reinterpret_cast<const u32*>(block.entrypoint);
             reinterpret_cast<const u8*>(pos) < reinterpret_cast<const u8*>(block.entrypoint) + block.size; pos += 1) {
            fmt::print("0x{:02x} 0x{:02x} ", reinterpret_cast<u64>(pos), *pos);
            fmt::print("{}", Common::DisassembleAArch64(*pos, reinterpret_cast<u64>(pos)));
            result += Common::DisassembleAArch64(*pos, reinterpret_cast<u64>(pos));        
        }
#endif
        return result;
    }

    void PerformCacheInvalidation() {
        if (invalidate_entire_cache) {
            jit_state.ResetRSB();
            block_of_code.ClearCache();
            emitter.ClearCache();

            invalid_cache_ranges.clear();
            invalidate_entire_cache = false;
            invalid_cache_generation++;
            return;
        }

        if (invalid_cache_ranges.empty()) {
            return;
        }

        jit_state.ResetRSB();
        emitter.InvalidateCacheRanges(invalid_cache_ranges);
        invalid_cache_ranges.clear();
        invalid_cache_generation++;
    }

    void RequestCacheInvalidation() {
        if (jit_interface->is_executing) {
            jit_state.halt_requested = true;
            return;
        }

        PerformCacheInvalidation();
    }

private:
    Jit* jit_interface;

    static CodePtr GetCurrentBlock(void* this_voidptr) {
        Jit::Impl& this_ = *static_cast<Jit::Impl*>(this_voidptr);
        A32JitState& jit_state = this_.jit_state;

        u32 pc = jit_state.Reg[15];
        A32::PSR cpsr{jit_state.Cpsr()};
        A32::FPSCR fpscr{jit_state.upper_location_descriptor};
        A32::LocationDescriptor descriptor{pc, cpsr, fpscr};

        return this_.GetBasicBlock(descriptor).entrypoint;
    }

    A32EmitA64::BlockDescriptor GetBasicBlock(IR::LocationDescriptor descriptor) {
        auto block = emitter.GetBasicBlock(descriptor);
        if (block)
            return *block;

        constexpr size_t MINIMUM_REMAINING_CODESIZE = 1 * 1024 * 1024;
        if (block_of_code.SpaceRemaining() < MINIMUM_REMAINING_CODESIZE) {
            invalidate_entire_cache = true;
            PerformCacheInvalidation();
        }

        IR::Block ir_block = A32::Translate(A32::LocationDescriptor{descriptor}, [this](u32 vaddr) { return config.callbacks->MemoryReadCode(vaddr); }, {config.define_unpredictable_behaviour, config.hook_hint_instructions});
        Optimization::A32GetSetElimination(ir_block);
        Optimization::DeadCodeElimination(ir_block);
        Optimization::A32ConstantMemoryReads(ir_block, config.callbacks);
        Optimization::ConstantPropagation(ir_block);
        Optimization::DeadCodeElimination(ir_block);
        Optimization::A32MergeInterpretBlocksPass(ir_block, config.callbacks);
        Optimization::VerificationPass(ir_block);
        return emitter.Emit(ir_block);
    }
};

Jit::Jit(UserConfig config) : impl(std::make_unique<Impl>(this, std::move(config))) {}

Jit::~Jit() = default;

void Jit::Run() {
    ASSERT(!is_executing);
    is_executing = true;
    SCOPE_EXIT { this->is_executing = false; };

    impl->jit_state.halt_requested = false;

    impl->Execute();

    impl->PerformCacheInvalidation();
}

void Jit::ClearCache() {
    impl->invalidate_entire_cache = true;
    impl->RequestCacheInvalidation();
}

void Jit::InvalidateCacheRange(std::uint32_t start_address, std::size_t length) {
    impl->invalid_cache_ranges.add(boost::icl::discrete_interval<u32>::closed(start_address, static_cast<u32>(start_address + length - 1)));
    impl->RequestCacheInvalidation();
}

void Jit::Reset() {
    ASSERT(!is_executing);
    impl->jit_state = {};
}

void Jit::HaltExecution() {
    impl->jit_state.halt_requested = true;
}

std::array<u32, 16>& Jit::Regs() {
    return impl->jit_state.Reg;
}
const std::array<u32, 16>& Jit::Regs() const {
    return impl->jit_state.Reg;
}

std::array<u32, 64>& Jit::ExtRegs() {
    return impl->jit_state.ExtReg;
}

const std::array<u32, 64>& Jit::ExtRegs() const {
    return impl->jit_state.ExtReg;
}

u32 Jit::Cpsr() const {
    return impl->jit_state.Cpsr();
}

void Jit::SetCpsr(u32 value) {
    return impl->jit_state.SetCpsr(value);
}

u32 Jit::Fpscr() const {
    return impl->jit_state.Fpscr();
}

void Jit::SetFpscr(u32 value) {
    return impl->jit_state.SetFpscr(value);
}

Context Jit::SaveContext() const {
    Context ctx;
    SaveContext(ctx);
    return ctx;
}

struct Context::Impl {
    A32JitState jit_state;
    size_t invalid_cache_generation;
};

Context::Context() : impl(std::make_unique<Context::Impl>()) { impl->jit_state.ResetRSB(); }
Context::~Context() = default;
Context::Context(const Context& ctx) : impl(std::make_unique<Context::Impl>(*ctx.impl)) {}
Context::Context(Context&& ctx) noexcept : impl(std::move(ctx.impl)) {}
Context& Context::operator=(const Context& ctx) {
    *impl = *ctx.impl;
    return *this;
}
Context& Context::operator=(Context&& ctx) noexcept {
    impl = std::move(ctx.impl);
    return *this;
}

std::array<std::uint32_t, 16>& Context::Regs() {
    return impl->jit_state.Reg;
}
const std::array<std::uint32_t, 16>& Context::Regs() const {
    return impl->jit_state.Reg;
}
std::array<std::uint32_t, 64>& Context::ExtRegs() {
    return impl->jit_state.ExtReg;
}
const std::array<std::uint32_t, 64>& Context::ExtRegs() const {
    return impl->jit_state.ExtReg;
}

std::uint32_t Context::Cpsr() const {
    return impl->jit_state.Cpsr();
}
void Context::SetCpsr(std::uint32_t value) {
    impl->jit_state.SetCpsr(value);
}

std::uint32_t Context::Fpscr() const {
    return impl->jit_state.Fpscr();
}
void Context::SetFpscr(std::uint32_t value) {
    return impl->jit_state.SetFpscr(value);
}

void Jit::SaveContext(Context& ctx) const {
    ctx.impl->jit_state.TransferJitState(impl->jit_state, false);
    ctx.impl->invalid_cache_generation = impl->invalid_cache_generation;
}

void Jit::LoadContext(const Context& ctx) {
    bool reset_rsb = ctx.impl->invalid_cache_generation != impl->invalid_cache_generation;
    impl->jit_state.TransferJitState(ctx.impl->jit_state, reset_rsb);
}

std::string Jit::Disassemble(const IR::LocationDescriptor& descriptor) {
    return impl->Disassemble(descriptor);
}

} // namespace Dynarmic::A32
