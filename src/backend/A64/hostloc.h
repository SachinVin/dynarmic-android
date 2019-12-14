/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */
#pragma once

#include "backend/A64/emitter/a64_emitter.h"
#include "common/assert.h"
#include "common/common_types.h"

namespace Dynarmic::BackendA64 {

enum class HostLoc {
    // Ordering of the registers is intentional. See also: HostLocToA64.

    // 64bit GPR registers
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,

    SP, // 64bit stack pointer

    // Qword FPR registers
    Q0,
    Q1,
    Q2,
    Q3,
    Q4,
    Q5,
    Q6,
    Q7,
    Q8,
    Q9,
    Q10,
    Q11,
    Q12,
    Q13,
    Q14,
    Q15,
    Q16,
    Q17,
    Q18,
    Q19,
    Q20,
    Q21,
    Q22,
    Q23,
    Q24,
    Q25,
    Q26,
    Q27,
    Q28,
    Q29,
    Q30,
    Q31,

    FirstSpill,
};

constexpr size_t NonSpillHostLocCount = static_cast<size_t>(HostLoc::FirstSpill);

inline bool HostLocIsGPR(HostLoc reg) {
    return reg >= HostLoc::X0 && reg <= HostLoc::X30;
}

inline bool HostLocIsFPR(HostLoc reg) {
    return reg >= HostLoc::Q0 && reg <= HostLoc::Q31;
}

inline bool HostLocIsRegister(HostLoc reg) {
    return HostLocIsGPR(reg) || HostLocIsFPR(reg);
}

inline HostLoc HostLocRegIdx(int idx) {
    ASSERT(idx >= 0 && idx <= 30);
    return static_cast<HostLoc>(idx);
}

inline HostLoc HostLocFprIdx(int idx) {
    ASSERT(idx >= 0 && idx <= 31);
    return static_cast<HostLoc>(static_cast<size_t>(HostLoc::Q0) + idx);
}

inline HostLoc HostLocSpill(size_t i) {
    return static_cast<HostLoc>(static_cast<size_t>(HostLoc::FirstSpill) + i);
}

inline bool HostLocIsSpill(HostLoc reg) {
    return reg >= HostLoc::FirstSpill;
}

inline size_t HostLocBitWidth(HostLoc loc) {
    if (HostLocIsGPR(loc))
        return 64;
    if (HostLocIsFPR(loc))
        return 128;
    if (HostLocIsSpill(loc))
        return 128;
    UNREACHABLE();
}

using HostLocList = std::initializer_list<HostLoc>;

// X18 may be reserved.(Windows and iOS)
// X26 holds the cycle counter
// X27 contains an emulated memory relate pointer
// X28 used for holding the JitState.
// X30 is the link register.
// In order of desireablity based first on ABI
constexpr HostLocList any_gpr = {
    HostLoc::X19, HostLoc::X20, HostLoc::X21, HostLoc::X22, HostLoc::X23,
    HostLoc::X24, HostLoc::X25,

    HostLoc::X8,  HostLoc::X9,  HostLoc::X10, HostLoc::X11, HostLoc::X12,
    HostLoc::X13, HostLoc::X14, HostLoc::X15, HostLoc::X16, HostLoc::X17,

    HostLoc::X7,  HostLoc::X6,  HostLoc::X5,  HostLoc::X4,  HostLoc::X3,
    HostLoc::X2,  HostLoc::X1,  HostLoc::X0,
};

constexpr HostLocList any_fpr = {
    HostLoc::Q8,  HostLoc::Q9,  HostLoc::Q10, HostLoc::Q11, HostLoc::Q12, HostLoc::Q13,
    HostLoc::Q14, HostLoc::Q15,

    HostLoc::Q16, HostLoc::Q17, HostLoc::Q18, HostLoc::Q19, HostLoc::Q20, HostLoc::Q21,
    HostLoc::Q22, HostLoc::Q23, HostLoc::Q24, HostLoc::Q25, HostLoc::Q26, HostLoc::Q27,
    HostLoc::Q28, HostLoc::Q29, HostLoc::Q30, HostLoc::Q31,

    HostLoc::Q7,  HostLoc::Q6,  HostLoc::Q5,  HostLoc::Q4,  HostLoc::Q3,  HostLoc::Q2,
    HostLoc::Q1,  HostLoc::Q0,
};

Arm64Gen::ARM64Reg HostLocToReg64(HostLoc loc);
Arm64Gen::ARM64Reg HostLocToFpr(HostLoc loc);

template <typename JitStateType>
size_t SpillToOpArg(HostLoc loc) {
    ASSERT(HostLocIsSpill(loc));

    size_t i = static_cast<size_t>(loc) - static_cast<size_t>(HostLoc::FirstSpill);
    ASSERT_MSG(i < JitStateType::SpillCount,
               "Spill index greater than number of available spill locations");

    return JitStateType::GetSpillLocationOffsetFromIndex(i);
}

} // namespace Dynarmic::BackendA64
