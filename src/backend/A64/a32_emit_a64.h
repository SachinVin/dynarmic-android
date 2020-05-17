/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#pragma once

#include <array>
#include <functional>
#include <optional>
#include <set>
#include <tuple>
#include <unordered_map>

#include "backend/A64/a32_jitstate.h"
#include "backend/A64/block_range_information.h"
#include "backend/A64/emit_a64.h"
#include "backend/A64/exception_handler.h"
#include "dynarmic/A32/a32.h"
#include "dynarmic/A32/config.h"
#include "frontend/A32/location_descriptor.h"
#include "frontend/ir/terminal.h"

namespace Dynarmic::BackendA64 {

struct A64State;
class RegAlloc;

struct A32EmitContext final : public EmitContext {
    A32EmitContext(RegAlloc& reg_alloc, IR::Block& block);
    A32::LocationDescriptor Location() const;
    bool IsSingleStep() const;
    FP::RoundingMode FPSCR_RMode() const override;
    u32 FPCR() const override;
    bool FPSCR_FTZ() const override;
    bool FPSCR_DN() const override;
    std::ptrdiff_t GetInstOffset(IR::Inst* inst) const;
};

class A32EmitA64 final : public EmitA64 {
public:
    A32EmitA64(BlockOfCode& code, A32::UserConfig config, A32::Jit* jit_interface);
    ~A32EmitA64() override;

    /**
     * Emit host machine code for a basic block with intermediate representation `ir`.
     * @note ir is modified.
     */
    BlockDescriptor Emit(IR::Block& ir);

    void ClearCache() override;

    void InvalidateCacheRanges(const boost::icl::interval_set<u32>& ranges);

    void FastmemCallback(CodePtr PC);

protected:
    const A32::UserConfig config;
    A32::Jit* jit_interface;
    BlockRangeInformation<u32> block_ranges;
    ExceptionHandler exception_handler;

    void EmitCondPrelude(const A32EmitContext& ctx);

    struct FastDispatchEntry {
        u64 location_descriptor = 0xFFFF'FFFF'FFFF'FFFFull;
        const void* code_ptr = nullptr;
    };
    static_assert(sizeof(FastDispatchEntry) == 0x10);
    static constexpr u64 fast_dispatch_table_mask = 0xFFFF0;
    static constexpr size_t fast_dispatch_table_size = 0x10000;
    std::array<FastDispatchEntry, fast_dispatch_table_size> fast_dispatch_table;
    void ClearFastDispatchTable();

    using DoNotFastmemMarker = std::tuple<IR::LocationDescriptor, std::ptrdiff_t>;
    std::set<DoNotFastmemMarker> do_not_fastmem;
    DoNotFastmemMarker GenerateDoNotFastmemMarker(A32EmitContext& ctx, IR::Inst* inst);
    void DoNotFastmem(const DoNotFastmemMarker& marker);
    bool ShouldFastmem(const DoNotFastmemMarker& marker) const;

    const void* read_memory_8;
    const void* read_memory_16;
    const void* read_memory_32;
    const void* read_memory_64;
    const void* write_memory_8;
    const void* write_memory_16;
    const void* write_memory_32;
    const void* write_memory_64;
    void GenMemoryAccessors();
    template<typename T>
    void ReadMemory(A32EmitContext& ctx, IR::Inst* inst, const CodePtr callback_fn);
    template<typename T>
    void WriteMemory(A32EmitContext& ctx, IR::Inst* inst, const CodePtr callback_fn);

    const void* terminal_handler_pop_rsb_hint;
    const void* terminal_handler_fast_dispatch_hint = nullptr;
    FastDispatchEntry& (*fast_dispatch_table_lookup)(u64) = nullptr;
    void GenTerminalHandlers();

    // Microinstruction emitters
#define OPCODE(...)
#define A32OPC(name, type, ...) void EmitA32##name(A32EmitContext& ctx, IR::Inst* inst);
#define A64OPC(...)
#include "frontend/ir/opcodes.inc"
#undef OPCODE
#undef A32OPC
#undef A64OPC

    // Helpers
    std::string LocationDescriptorToFriendlyName(const IR::LocationDescriptor&) const override;

    // Fastmem
    struct FastmemPatchInfo {
        std::function<void()> callback;
    };
    std::unordered_map<CodePtr, FastmemPatchInfo> fastmem_patch_info;

    // Terminal instruction emitters
    void EmitSetUpperLocationDescriptor(IR::LocationDescriptor new_location, IR::LocationDescriptor old_location);
    void EmitTerminalImpl(IR::Term::Interpret terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::ReturnToDispatch terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::LinkBlock terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::LinkBlockFast terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::PopRSBHint terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::FastDispatchHint terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::If terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::CheckBit terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;
    void EmitTerminalImpl(IR::Term::CheckHalt terminal, IR::LocationDescriptor initial_location, bool is_single_step) override;

    // Patching
    void Unpatch(const IR::LocationDescriptor& target_desc) override;
    void EmitPatchJg(const IR::LocationDescriptor& target_desc, CodePtr target_code_ptr = nullptr) override;
    void EmitPatchJmp(const IR::LocationDescriptor& target_desc, CodePtr target_code_ptr = nullptr) override;
    void EmitPatchMovX0(CodePtr target_code_ptr = nullptr) override;
};

} // namespace Dynarmic::BackendA64
