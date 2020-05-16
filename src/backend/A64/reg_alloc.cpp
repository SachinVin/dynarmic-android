/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <algorithm>
#include <numeric>
#include <utility>

#include <fmt/ostream.h>

#include "backend/A64/abi.h"
#include "backend/A64/reg_alloc.h"
#include "common/assert.h"

namespace Dynarmic::BackendA64 {

static u64 ImmediateToU64(const IR::Value& imm) {
    switch (imm.GetType()) {
    case IR::Type::U1:
        return u64(imm.GetU1());
    case IR::Type::U8:
        return u64(imm.GetU8());
    case IR::Type::U16:
        return u64(imm.GetU16());
    case IR::Type::U32:
        return u64(imm.GetU32());
    case IR::Type::U64:
        return u64(imm.GetU64());
    default:
        ASSERT_FALSE("This should never happen.");
    }
}

static bool CanExchange(HostLoc a, HostLoc b) {
    return HostLocIsGPR(a) && HostLocIsGPR(b);
}

// Minimum number of bits required to represent a type
static size_t GetBitWidth(IR::Type type) {
    switch (type) {
    case IR::Type::A32Reg:
    case IR::Type::A32ExtReg:
    case IR::Type::A64Reg:
    case IR::Type::A64Vec:
    case IR::Type::CoprocInfo:
    case IR::Type::Cond:
    case IR::Type::Void:
    case IR::Type::Table:
        ASSERT_FALSE("Type {} cannot be represented at runtime", type);
        return 0;
    case IR::Type::Opaque:
        ASSERT_FALSE("Not a concrete type");
        return 0;
    case IR::Type::U1:
        return 8;
    case IR::Type::U8:
        return 8;
    case IR::Type::U16:
        return 16;
    case IR::Type::U32:
        return 32;
    case IR::Type::U64:
        return 64;
    case IR::Type::U128:
        return 128;
    case IR::Type::NZCVFlags:
        return 32; // TODO: Update to 16 when flags optimization is done
    }
    UNREACHABLE();
    return 0;
}

static bool IsValuelessType(IR::Type type) {
    switch (type) {
    case IR::Type::Table:
        return true;
    default:
        return false;
    }
}

bool HostLocInfo::IsLocked() const {
    return is_being_used_count > 0;
}

bool HostLocInfo::IsEmpty() const {
    return is_being_used_count == 0 && values.empty();
}

bool HostLocInfo::IsLastUse() const {
    return is_being_used_count == 0 && current_references == 1 && accumulated_uses + 1 == total_uses;
}

void HostLocInfo::ReadLock() {
    ASSERT(!is_scratch);
    is_being_used_count++;
}

void HostLocInfo::WriteLock() {
    ASSERT(is_being_used_count == 0);
    is_being_used_count++;
    is_scratch = true;
}

void HostLocInfo::AddArgReference() {
    current_references++;
    ASSERT(accumulated_uses + current_references <= total_uses);
}

void HostLocInfo::ReleaseOne() {
    is_being_used_count--;
    is_scratch = false;

    if (current_references == 0)
        return;

    accumulated_uses++;
    current_references--;

    if (current_references == 0)
        ReleaseAll();
}

void HostLocInfo::ReleaseAll() {
    accumulated_uses += current_references;
    current_references = 0;

    ASSERT(total_uses == std::accumulate(values.begin(), values.end(), size_t(0), [](size_t sum, IR::Inst* inst) { return sum + inst->UseCount(); }));

    if (total_uses == accumulated_uses) {
        values.clear();
        accumulated_uses = 0;
        total_uses = 0;
        max_bit_width = 0;
    }

    is_being_used_count = 0;
    is_scratch = false;
}

bool HostLocInfo::ContainsValue(const IR::Inst* inst) const {
    return std::find(values.begin(), values.end(), inst) != values.end();
}

size_t HostLocInfo::GetMaxBitWidth() const {
    return max_bit_width;
}

void HostLocInfo::AddValue(IR::Inst* inst) {
    values.push_back(inst);
    total_uses += inst->UseCount();
    max_bit_width = std::max(max_bit_width, GetBitWidth(inst->GetType()));
}

IR::Type Argument::GetType() const {
    return value.GetType();
}

bool Argument::IsImmediate() const {
    return value.IsImmediate();
}

bool Argument::IsVoid() const {
    return GetType() == IR::Type::Void;
}

bool Argument::FitsInImmediateU32() const {
    if (!IsImmediate())
        return false;
    u64 imm = ImmediateToU64(value);
    return imm < 0x100000000;
}

bool Argument::FitsInImmediateS32() const {
    if (!IsImmediate())
        return false;
    s64 imm = static_cast<s64>(ImmediateToU64(value));
    return -s64(0x80000000) <= imm && imm <= s64(0x7FFFFFFF);
}

bool Argument::GetImmediateU1() const {
    return value.GetU1();
}

u8 Argument::GetImmediateU8() const {
    u64 imm = ImmediateToU64(value);
    ASSERT(imm < 0x100);
    return u8(imm);
}

u16 Argument::GetImmediateU16() const {
    u64 imm = ImmediateToU64(value);
    ASSERT(imm < 0x10000);
    return u16(imm);
}

u32 Argument::GetImmediateU32() const {
    u64 imm = ImmediateToU64(value);
    ASSERT(imm < 0x100000000);
    return u32(imm);
}

u64 Argument::GetImmediateS32() const {
    ASSERT(FitsInImmediateS32());
    u64 imm = ImmediateToU64(value);
    return imm;
}

u64 Argument::GetImmediateU64() const {
    return ImmediateToU64(value);
}

IR::Cond Argument::GetImmediateCond() const {
    ASSERT(IsImmediate() && GetType() == IR::Type::Cond);
    return value.GetCond();
}

bool Argument::IsInGpr() const {
    if (IsImmediate())
        return false;
    return HostLocIsGPR(*reg_alloc.ValueLocation(value.GetInst()));
}

bool Argument::IsInFpr() const {
    if (IsImmediate())
        return false;
    return HostLocIsFPR(*reg_alloc.ValueLocation(value.GetInst()));
}

bool Argument::IsInMemory() const {
    if (IsImmediate())
        return false;
    return HostLocIsSpill(*reg_alloc.ValueLocation(value.GetInst()));
}

RegAlloc::ArgumentInfo RegAlloc::GetArgumentInfo(IR::Inst* inst) {
    ArgumentInfo ret = {Argument{*this}, Argument{*this}, Argument{*this}, Argument{*this}};
    for (size_t i = 0; i < inst->NumArgs(); i++) {
        const IR::Value& arg = inst->GetArg(i);
        ret[i].value = arg;
        if (!arg.IsImmediate() && !IsValuelessType(arg.GetType())) {
            ASSERT_MSG(ValueLocation(arg.GetInst()), "argument must already been defined");
            LocInfo(*ValueLocation(arg.GetInst())).AddArgReference();
        }
    }
    return ret;
}

Arm64Gen::ARM64Reg RegAlloc::UseGpr(Argument& arg) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    return HostLocToReg64(UseImpl(arg.value, any_gpr));
}

Arm64Gen::ARM64Reg RegAlloc::UseFpr(Argument& arg) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    return HostLocToFpr(UseImpl(arg.value, any_fpr));
}

//OpArg RegAlloc::UseOpArg(Argument& arg) {
//    return UseGpr(arg);
//}

void RegAlloc::Use(Argument& arg, HostLoc host_loc) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    UseImpl(arg.value, {host_loc});
}

Arm64Gen::ARM64Reg RegAlloc::UseScratchGpr(Argument& arg) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    return HostLocToReg64(UseScratchImpl(arg.value, any_gpr));
}

Arm64Gen::ARM64Reg RegAlloc::UseScratchFpr(Argument& arg) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    return HostLocToFpr(UseScratchImpl(arg.value, any_fpr));
}

void RegAlloc::UseScratch(Argument& arg, HostLoc host_loc) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    UseScratchImpl(arg.value, {host_loc});
}

void RegAlloc::DefineValue(IR::Inst* inst, const Arm64Gen::ARM64Reg& reg) {
    ASSERT(IsVector(reg) || IsGPR(reg));
    HostLoc hostloc = static_cast<HostLoc>(DecodeReg(reg) + static_cast<size_t>(IsVector(reg) ? HostLoc::Q0 : HostLoc::X0));
    DefineValueImpl(inst, hostloc);
}

void RegAlloc::DefineValue(IR::Inst* inst, Argument& arg) {
    ASSERT(!arg.allocated);
    arg.allocated = true;
    DefineValueImpl(inst, arg.value);
}

void RegAlloc::Release(const Arm64Gen::ARM64Reg& reg) {
    ASSERT(IsVector(reg) || IsGPR(reg));
    const HostLoc hostloc = static_cast<HostLoc>(DecodeReg(reg) + static_cast<size_t>(IsVector(reg) ? HostLoc::Q0 : HostLoc::X0));
    LocInfo(hostloc).ReleaseOne();
}

Arm64Gen::ARM64Reg RegAlloc::ScratchGpr(HostLocList desired_locations) {
    return HostLocToReg64(ScratchImpl(desired_locations));
}

Arm64Gen::ARM64Reg RegAlloc::ScratchFpr(HostLocList desired_locations) {
    return HostLocToFpr(ScratchImpl(desired_locations));
}

HostLoc RegAlloc::UseImpl(IR::Value use_value, HostLocList desired_locations) {
    if (use_value.IsImmediate()) {
        return LoadImmediate(use_value, ScratchImpl(desired_locations));
    }

    const IR::Inst* use_inst = use_value.GetInst();
    const HostLoc current_location = *ValueLocation(use_inst);
    const size_t max_bit_width = LocInfo(current_location).GetMaxBitWidth();

    const bool can_use_current_location = std::find(desired_locations.begin(), desired_locations.end(), current_location) != desired_locations.end();
    if (can_use_current_location) {
        LocInfo(current_location).ReadLock();
        return current_location;
    }

    if (LocInfo(current_location).IsLocked()) {
        return UseScratchImpl(use_value, desired_locations);
    }

    const HostLoc destination_location = SelectARegister(desired_locations);
    if (max_bit_width > HostLocBitWidth(destination_location)) {
        return UseScratchImpl(use_value, desired_locations);
    } else if (CanExchange(destination_location, current_location)) {
        Exchange(destination_location, current_location);
    } else {
        MoveOutOfTheWay(destination_location);
        Move(destination_location, current_location);
    }
    LocInfo(destination_location).ReadLock();
    return destination_location;
}

HostLoc RegAlloc::UseScratchImpl(IR::Value use_value, HostLocList desired_locations) {
    if (use_value.IsImmediate()) {
        return LoadImmediate(use_value, ScratchImpl(desired_locations));
    }

    const IR::Inst* use_inst = use_value.GetInst();
    const HostLoc current_location = *ValueLocation(use_inst);
    const size_t bit_width = GetBitWidth(use_inst->GetType());

    const bool can_use_current_location = std::find(desired_locations.begin(), desired_locations.end(), current_location) != desired_locations.end();
    if (can_use_current_location && !LocInfo(current_location).IsLocked()) {
        if (!LocInfo(current_location).IsLastUse()) {
            MoveOutOfTheWay(current_location);
        }
        LocInfo(current_location).WriteLock();
        return current_location;
    }

    const HostLoc destination_location = SelectARegister(desired_locations);
    MoveOutOfTheWay(destination_location);
    CopyToScratch(bit_width, destination_location, current_location);
    LocInfo(destination_location).WriteLock();
    return destination_location;
}

HostLoc RegAlloc::ScratchImpl(HostLocList desired_locations) {
    HostLoc location = SelectARegister(desired_locations);
    MoveOutOfTheWay(location);
    LocInfo(location).WriteLock();
    return location;
}

void RegAlloc::HostCall(IR::Inst* result_def, std::optional<Argument::copyable_reference> arg0, 
                        std::optional<Argument::copyable_reference> arg1,
                        std::optional<Argument::copyable_reference> arg2, 
                        std::optional<Argument::copyable_reference> arg3, 
                        std::optional<Argument::copyable_reference> arg4, 
                        std::optional<Argument::copyable_reference> arg5, 
                        std::optional<Argument::copyable_reference> arg6, 
                        std::optional<Argument::copyable_reference> arg7) {
    constexpr size_t args_count = 8;
    constexpr std::array<HostLoc, args_count> args_hostloc = { ABI_PARAM1, ABI_PARAM2, ABI_PARAM3, ABI_PARAM4, ABI_PARAM5, ABI_PARAM6, ABI_PARAM7, ABI_PARAM8 };
    const std::array<std::optional<Argument::copyable_reference>, args_count> args = {arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7};

    static const std::vector<HostLoc> other_caller_save = [args_hostloc]() {
        std::vector<HostLoc> ret(ABI_ALL_CALLER_SAVE.begin(), ABI_ALL_CALLER_SAVE.end());

        for (auto hostloc : args_hostloc)
            ret.erase(std::find(ret.begin(), ret.end(), hostloc));

        return ret;
    }();

    for (size_t i = 0; i < args_count; i++) {
        if (args[i]) {
            UseScratch(*args[i], args_hostloc[i]);
        }
    }

    for (size_t i = 0; i < args_count; i++) {
        if (!args[i]) {
            // TODO: Force spill
            ScratchGpr({args_hostloc[i]});
        }
    }

    for (HostLoc caller_saved : other_caller_save) {
        ScratchImpl({caller_saved});
    }

     if (result_def) {
        DefineValueImpl(result_def, ABI_RETURN);
    }
}

void RegAlloc::EndOfAllocScope() {
    for (auto& iter : hostloc_info) {
        iter.ReleaseAll();
    }
}

void RegAlloc::AssertNoMoreUses() {
    ASSERT(std::all_of(hostloc_info.begin(), hostloc_info.end(), [](const auto& i) { return i.IsEmpty(); }));
}

HostLoc RegAlloc::SelectARegister(HostLocList desired_locations) const {
     std::vector<HostLoc> candidates = desired_locations;

    // Find all locations that have not been allocated..
    const auto allocated_locs = std::partition(candidates.begin(), candidates.end(), [this](auto loc){
        return !this->LocInfo(loc).IsLocked();
    });
    candidates.erase(allocated_locs, candidates.end());
    ASSERT_MSG(!candidates.empty(), "All candidate registers have already been allocated");

    // Selects the best location out of the available locations.
    // TODO: Actually do LRU or something. Currently we just try to pick something without a value if possible.

    std::partition(candidates.begin(), candidates.end(), [this](auto loc){
        return this->LocInfo(loc).IsEmpty();
    });

    return candidates.front();
}

std::optional<HostLoc> RegAlloc::ValueLocation(const IR::Inst* value) const {
    for (size_t i = 0; i < hostloc_info.size(); i++)
        if (hostloc_info[i].ContainsValue(value))
            return static_cast<HostLoc>(i);

    return std::nullopt;
}

void RegAlloc::DefineValueImpl(IR::Inst* def_inst, HostLoc host_loc) {
    ASSERT_MSG(!ValueLocation(def_inst), "def_inst has already been defined");
    LocInfo(host_loc).AddValue(def_inst);
}

void RegAlloc::DefineValueImpl(IR::Inst* def_inst, const IR::Value& use_inst) {
    ASSERT_MSG(!ValueLocation(def_inst), "def_inst has already been defined");

    if (use_inst.IsImmediate()) {
        HostLoc location = ScratchImpl(any_gpr);
        DefineValueImpl(def_inst, location);
        LoadImmediate(use_inst, location);
        return;
    }

    ASSERT_MSG(ValueLocation(use_inst.GetInst()), "use_inst must already be defined");
    HostLoc location = *ValueLocation(use_inst.GetInst());
    DefineValueImpl(def_inst, location);
}

HostLoc RegAlloc::LoadImmediate(IR::Value imm, HostLoc host_loc) {
    ASSERT_MSG(imm.IsImmediate(), "imm is not an immediate");

    if (HostLocIsGPR(host_loc)) {
        Arm64Gen::ARM64Reg reg = HostLocToReg64(host_loc);
        u64 imm_value = ImmediateToU64(imm);
        code.MOVI2R(reg, imm_value);
        return host_loc;
    }

    if (HostLocIsFPR(host_loc)) {
        Arm64Gen::ARM64Reg reg = Arm64Gen::EncodeRegToDouble(HostLocToFpr(host_loc));
        u64 imm_value = ImmediateToU64(imm);
        if (imm_value == 0)
            code.fp_emitter.FMOV(reg, 0);
        else {
            code.EmitPatchLDR(reg, imm_value);
        }
        return host_loc;
    }

    UNREACHABLE();
}

void RegAlloc::Move(HostLoc to, HostLoc from) {
    const size_t bit_width = LocInfo(from).GetMaxBitWidth();

    ASSERT(LocInfo(to).IsEmpty() && !LocInfo(from).IsLocked());
    ASSERT(bit_width <= HostLocBitWidth(to));

    if (LocInfo(from).IsEmpty()) {
        return;
    }

    EmitMove(bit_width, to, from);

    LocInfo(to) = std::exchange(LocInfo(from), {});
}

void RegAlloc::CopyToScratch(size_t bit_width, HostLoc to, HostLoc from) {
    ASSERT(LocInfo(to).IsEmpty() && !LocInfo(from).IsEmpty());

    EmitMove(bit_width, to, from);
}

void RegAlloc::Exchange(HostLoc a, HostLoc b) {
    ASSERT(!LocInfo(a).IsLocked() && !LocInfo(b).IsLocked());
    ASSERT(LocInfo(a).GetMaxBitWidth() <= HostLocBitWidth(b));
    ASSERT(LocInfo(b).GetMaxBitWidth() <= HostLocBitWidth(a));

    if (LocInfo(a).IsEmpty()) {
        Move(a, b);
        return;
    }

    if (LocInfo(b).IsEmpty()) {
        Move(b, a);
        return;
    }

    EmitExchange(a, b);

    std::swap(LocInfo(a), LocInfo(b));
}

void RegAlloc::MoveOutOfTheWay(HostLoc reg) {
    ASSERT(!LocInfo(reg).IsLocked());
    if (!LocInfo(reg).IsEmpty()) {
        SpillRegister(reg);
    }
}

void RegAlloc::SpillRegister(HostLoc loc) {
    ASSERT_MSG(HostLocIsRegister(loc), "Only registers can be spilled");
    ASSERT_MSG(!LocInfo(loc).IsEmpty(), "There is no need to spill unoccupied registers");
    ASSERT_MSG(!LocInfo(loc).IsLocked(), "Registers that have been allocated must not be spilt");

    HostLoc new_loc = FindFreeSpill();
    Move(new_loc, loc);
}

HostLoc RegAlloc::FindFreeSpill() const {
    for (size_t i = static_cast<size_t>(HostLoc::FirstSpill); i < hostloc_info.size(); i++) {
        HostLoc loc = static_cast<HostLoc>(i);
        if (LocInfo(loc).IsEmpty())
            return loc;
    }

    ASSERT_FALSE("All spill locations are full");
}

HostLocInfo& RegAlloc::LocInfo(HostLoc loc) {
    ASSERT(loc != HostLoc::SP && loc != HostLoc::X28 && loc != HostLoc::X29 && loc != HostLoc::X30);
    return hostloc_info[static_cast<size_t>(loc)];
}

const HostLocInfo& RegAlloc::LocInfo(HostLoc loc) const {
    ASSERT(loc != HostLoc::SP && loc != HostLoc::X28 && loc != HostLoc::X29 && loc != HostLoc::X30);
    return hostloc_info[static_cast<size_t>(loc)];
}

void RegAlloc::EmitMove(size_t bit_width, HostLoc to, HostLoc from) {
    if (HostLocIsFPR(to) && HostLocIsFPR(from)) {
        // bit_width == 128
        //mov(HostLocToFpr(to), HostLocToFpr(from));

        ASSERT_FALSE("Unimplemented");
    } else if (HostLocIsGPR(to) && HostLocIsGPR(from)) {
        ASSERT(bit_width != 128);
        if (bit_width == 64) {
            code.MOV(HostLocToReg64(to), HostLocToReg64(from));
        } else {
            code.MOV(DecodeReg(HostLocToReg64(to)), DecodeReg(HostLocToReg64(from)));
        }
    } else if (HostLocIsFPR(to) && HostLocIsGPR(from)) {
        ASSERT(bit_width != 128);
        if (bit_width == 64) {
            code.fp_emitter.FMOV(EncodeRegToDouble(HostLocToFpr(to)), HostLocToReg64(from));
        } else {
            code.fp_emitter.FMOV(EncodeRegToSingle(HostLocToFpr(to)), DecodeReg(HostLocToReg64(from)));
        }
    } else if (HostLocIsGPR(to) && HostLocIsFPR(from)) {
        ASSERT(bit_width != 128);
        if (bit_width == 64) {
            code.fp_emitter.FMOV(HostLocToReg64(to), EncodeRegToDouble(HostLocToFpr(from)));
        } else {
            code.fp_emitter.FMOV(DecodeReg(HostLocToReg64(to)), EncodeRegToSingle(HostLocToFpr(from)));
        }
    } else if (HostLocIsFPR(to) && HostLocIsSpill(from)) {
        s32 spill_addr = spill_to_addr(from);
        // ASSERT(spill_addr.getBit() >= bit_width);
        code.fp_emitter.LDR(bit_width, Arm64Gen::INDEX_UNSIGNED, HostLocToFpr(to), Arm64Gen::X28, spill_addr);
    } else if (HostLocIsSpill(to) && HostLocIsFPR(from)) {
        s32 spill_addr = spill_to_addr(to);
        // ASSERT(spill_addr.getBit() >= bit_width);
        code.fp_emitter.STR(bit_width, Arm64Gen::INDEX_UNSIGNED, HostLocToFpr(from), Arm64Gen::X28, spill_addr);
    } else if (HostLocIsGPR(to) && HostLocIsSpill(from)) {
        ASSERT(bit_width != 128);
        if (bit_width == 64) {
            code.LDR(Arm64Gen::INDEX_UNSIGNED, HostLocToReg64(to), Arm64Gen::X28, spill_to_addr(from));
        } else {
            code.LDR(Arm64Gen::INDEX_UNSIGNED, DecodeReg(HostLocToReg64(to)), Arm64Gen::X28, spill_to_addr(from));
        }
    } else if (HostLocIsSpill(to) && HostLocIsGPR(from)) {
        ASSERT(bit_width != 128);
        if (bit_width == 64) {
            code.STR(Arm64Gen::INDEX_UNSIGNED, HostLocToReg64(from), Arm64Gen::X28, spill_to_addr(to));
        } else {
            code.STR(Arm64Gen::INDEX_UNSIGNED, DecodeReg(HostLocToReg64(from)), Arm64Gen::X28, spill_to_addr(to));
        }
    } else {
        ASSERT_FALSE("Invalid RegAlloc::EmitMove");
    }
}

void RegAlloc::EmitExchange(HostLoc a, HostLoc b) {
    if (HostLocIsGPR(a) && HostLocIsGPR(b)) {
        // Is this the best way to do it?
        code.EOR(HostLocToReg64(a), HostLocToReg64(a), HostLocToReg64(b));
        code.EOR(HostLocToReg64(b), HostLocToReg64(a), HostLocToReg64(b));
        code.EOR(HostLocToReg64(a), HostLocToReg64(a), HostLocToReg64(b));
    } else if (HostLocIsFPR(a) && HostLocIsFPR(b)) {
        ASSERT_FALSE("Check your code: Exchanging XMM registers is unnecessary");
    } else {
        ASSERT_FALSE("Invalid RegAlloc::EmitExchange");
    }
}

} // namespace Dynarmic::BackendA64
