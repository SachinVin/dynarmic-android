/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */
#pragma once

#include <array>

#include "backend/A64/block_of_code.h"
#include "backend/A64/hostloc.h"

namespace Dynarmic::BackendA64 {

constexpr HostLoc ABI_RETURN = HostLoc::X0;

constexpr HostLoc ABI_PARAM1 = HostLoc::X0;
constexpr HostLoc ABI_PARAM2 = HostLoc::X1;
constexpr HostLoc ABI_PARAM3 = HostLoc::X2;
constexpr HostLoc ABI_PARAM4 = HostLoc::X3;
constexpr HostLoc ABI_PARAM5 = HostLoc::X4;
constexpr HostLoc ABI_PARAM6 = HostLoc::X5;
constexpr HostLoc ABI_PARAM7 = HostLoc::X6;
constexpr HostLoc ABI_PARAM8 = HostLoc::X7;

constexpr std::array<HostLoc, 43> ABI_ALL_CALLER_SAVE = {  
    HostLoc::X0,
    HostLoc::X1,
    HostLoc::X2,
    HostLoc::X3,
    HostLoc::X4,
    HostLoc::X5,
    HostLoc::X6,
    HostLoc::X7,
    HostLoc::X8,
    HostLoc::X9,
    HostLoc::X10,
    HostLoc::X11,
    HostLoc::X12,
    HostLoc::X13,
    HostLoc::X14,
    HostLoc::X15,
    HostLoc::X16,
    HostLoc::X17,
    HostLoc::X18,

    HostLoc::Q0,
    HostLoc::Q1,
    HostLoc::Q2,
    HostLoc::Q3,
    HostLoc::Q4,
    HostLoc::Q5,
    HostLoc::Q6,
    HostLoc::Q7,
   
    HostLoc::Q16,
    HostLoc::Q17,
    HostLoc::Q18,
    HostLoc::Q19,
    HostLoc::Q20,
    HostLoc::Q21,
    HostLoc::Q22,
    HostLoc::Q23,
    HostLoc::Q24,
    HostLoc::Q25,
    HostLoc::Q26,
    HostLoc::Q27,
    HostLoc::Q28,
    HostLoc::Q29,
    HostLoc::Q30,
    HostLoc::Q31,
};

constexpr std::array<HostLoc, 20> ABI_ALL_CALLEE_SAVE = {
    HostLoc::X19,
    HostLoc::X20,
    HostLoc::X21,
    HostLoc::X22,
    HostLoc::X23,
    HostLoc::X24,
    HostLoc::X25,
    HostLoc::X26,
    HostLoc::X27,
    HostLoc::X28,
    HostLoc::X29,
    HostLoc::X30,

    HostLoc::Q8,
    HostLoc::Q9,
    HostLoc::Q10,
    HostLoc::Q11,
    HostLoc::Q12,
    HostLoc::Q13,
    HostLoc::Q14,
    HostLoc::Q15,
};

constexpr size_t ABI_SHADOW_SPACE = 0; // bytes

static_assert(ABI_ALL_CALLER_SAVE.size() + ABI_ALL_CALLEE_SAVE.size() == 63, "Invalid total number of registers");

void ABI_PushCalleeSaveRegistersAndAdjustStack(BlockOfCode& code);
void ABI_PopCalleeSaveRegistersAndAdjustStack(BlockOfCode& code);
void ABI_PushCallerSaveRegistersAndAdjustStack(BlockOfCode& code);
void ABI_PopCallerSaveRegistersAndAdjustStack(BlockOfCode& code);

void ABI_PushCallerSaveRegistersAndAdjustStackExcept(BlockOfCode& code, HostLoc exception);
void ABI_PopCallerSaveRegistersAndAdjustStackExcept(BlockOfCode& code, HostLoc exception);

} // namespace Dynarmic::BackendX64
