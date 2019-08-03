/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#pragma once

#include <array>
#include <functional>
#include <utility>
#include <vector>

#include <optional>

#include "backend/A64/block_of_code.h"
#include "backend/A64/hostloc.h"
//#include "backend/A64/oparg.h"
#include "common/common_types.h"
#include "frontend/ir/cond.h"
#include "frontend/ir/microinstruction.h"
#include "frontend/ir/value.h"

namespace Dynarmic::BackendA64 {

class RegAlloc;

struct HostLocInfo {
public:
    bool IsLocked() const;
    bool IsEmpty() const;
    bool IsLastUse() const;

    void ReadLock();
    void WriteLock();
    void AddArgReference();
    void ReleaseOne();
    void ReleaseAll();

    bool ContainsValue(const IR::Inst* inst) const;
    size_t GetMaxBitWidth() const;

    void AddValue(IR::Inst* inst);

private:
    // Current instruction state
    size_t is_being_used_count = 0;
    bool is_scratch = false;

    // Block state
    size_t current_references = 0;
    size_t accumulated_uses = 0;
    size_t total_uses = 0;

    // Value state
    std::vector<IR::Inst*> values;
    size_t max_bit_width = 0;
};

struct Argument {
public:
    using copyable_reference = std::reference_wrapper<Argument>;

    IR::Type GetType() const;
    bool IsImmediate() const;
    bool IsVoid() const;

    bool FitsInImmediateU32() const;
    bool FitsInImmediateS32() const;

    bool GetImmediateU1() const;
    u8 GetImmediateU8() const;
    u16 GetImmediateU16() const;
    u32 GetImmediateU32() const;
    u64 GetImmediateS32() const;
    u64 GetImmediateU64() const;
    IR::Cond GetImmediateCond() const;

    /// Is this value currently in a GPR?
    bool IsInGpr() const;
    /// Is this value currently in a FPR?
    bool IsInFpr() const;
    /// Is this value currently in memory?
    bool IsInMemory() const;

private:
    friend class RegAlloc;
    explicit Argument(RegAlloc& reg_alloc) : reg_alloc(reg_alloc) {}

    bool allocated = false;
    RegAlloc& reg_alloc;
    IR::Value value;
};

class RegAlloc final {
public:
    using ArgumentInfo = std::array<Argument, IR::max_arg_count>;

    explicit RegAlloc(BlockOfCode& code, size_t num_spills, std::function<u64(HostLoc)> spill_to_addr)
        : hostloc_info(NonSpillHostLocCount + num_spills), code(code), spill_to_addr(std::move(spill_to_addr)) {}

    ArgumentInfo GetArgumentInfo(IR::Inst* inst);

    Arm64Gen::ARM64Reg UseGpr(Argument& arg);
    Arm64Gen::ARM64Reg UseFpr(Argument& arg);
    //OpArg UseOpArg(Argument& arg);
    void Use(Argument& arg, HostLoc host_loc);

    Arm64Gen::ARM64Reg UseScratchGpr(Argument& arg);
    Arm64Gen::ARM64Reg UseScratchFpr(Argument& arg);
    void UseScratch(Argument& arg, HostLoc host_loc);

    void DefineValue(IR::Inst* inst, const Arm64Gen::ARM64Reg& reg);
    void DefineValue(IR::Inst* inst, Argument& arg);

    void Release(const Arm64Gen::ARM64Reg& reg);

    Arm64Gen::ARM64Reg ScratchGpr(HostLocList desired_locations = any_gpr);
    Arm64Gen::ARM64Reg ScratchFpr(HostLocList desired_locations = any_fpr);

    void HostCall(IR::Inst* result_def = nullptr, std::optional<Argument::copyable_reference> arg0 = {},
                  std::optional<Argument::copyable_reference> arg1 = {}, 
                  std::optional<Argument::copyable_reference> arg2 = {},
                  std::optional<Argument::copyable_reference> arg3 = {},
                  std::optional<Argument::copyable_reference> arg4 = {},
                  std::optional<Argument::copyable_reference> arg5 = {},
                  std::optional<Argument::copyable_reference> arg6 = {},
                  std::optional<Argument::copyable_reference> arg7 = {});

    // TODO: Values in host flags

    void EndOfAllocScope();

    void AssertNoMoreUses();

private:
    friend struct Argument;

    HostLoc SelectARegister(HostLocList desired_locations) const;
    std::optional<HostLoc> ValueLocation(const IR::Inst* value) const;

    HostLoc UseImpl(IR::Value use_value, HostLocList desired_locations);
    HostLoc UseScratchImpl(IR::Value use_value, HostLocList desired_locations);
    HostLoc ScratchImpl(HostLocList desired_locations);
    void DefineValueImpl(IR::Inst* def_inst, HostLoc host_loc);
    void DefineValueImpl(IR::Inst* def_inst, const IR::Value& use_inst);

    HostLoc LoadImmediate(IR::Value imm, HostLoc reg);
    void Move(HostLoc to, HostLoc from);
    void CopyToScratch(size_t bit_width, HostLoc to, HostLoc from);
    void Exchange(HostLoc a, HostLoc b);
    void MoveOutOfTheWay(HostLoc reg);

    void SpillRegister(HostLoc loc);
    HostLoc FindFreeSpill() const;

    std::vector<HostLocInfo> hostloc_info;
    HostLocInfo& LocInfo(HostLoc loc);
    const HostLocInfo& LocInfo(HostLoc loc) const;

    BlockOfCode& code;
    std::function<u32(HostLoc)> spill_to_addr;
    void EmitMove(size_t bit_width, HostLoc to, HostLoc from);
    void EmitExchange(HostLoc a, HostLoc b);
};

} // namespace Dynarmic::BackendA64
