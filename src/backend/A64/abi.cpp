// Copyright (C) 2003 Dolphin Project.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 2.0 or later versions.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License 2.0 for more details.

// A copy of the GPL 2.0 should have been included with the program.
// If not, see http://www.gnu.org/licenses/

// 20th Sep 2018: This code was modified for Dynarmic.

#include <algorithm>
#include <vector>

#include "backend/A64/abi.h"
#include "common/common_types.h"
#include "common/math_util.h"
#include "common/iterator_util.h"

namespace Dynarmic::BackendA64 {

template<typename RegisterArrayT>
void ABI_PushRegistersAndAdjustStack(BlockOfCode& code, const RegisterArrayT& regs) {
    u32 gprs = 0 , fprs = 0;

    for (HostLoc reg : regs) {
        if (HostLocIsGPR(reg)) {
            gprs |= 0x1 << static_cast<u32>(DecodeReg(HostLocToReg64(reg)));
        } else if (HostLocIsFPR(reg)) {
            fprs |= 0x1 << static_cast<u32>(DecodeReg(HostLocToFpr(reg)));
        }
    }

    code.fp_emitter.ABI_PushRegisters(fprs);
    code.ABI_PushRegisters(gprs);
}

template<typename RegisterArrayT>
void ABI_PopRegistersAndAdjustStack(BlockOfCode& code, const RegisterArrayT& regs) {
    u32 gprs = 0, fprs = 0;

    for (HostLoc reg : regs) {
        if (HostLocIsGPR(reg)) {
            gprs |= 0x1 << static_cast<u32>(DecodeReg(HostLocToReg64(reg)));
        } else if (HostLocIsFPR(reg)) {
            fprs |= 0x1 << static_cast<u32>(DecodeReg(HostLocToFpr(reg)));
        }
    }

    code.ABI_PopRegisters(gprs);
    code.fp_emitter.ABI_PopRegisters(fprs);
}

void ABI_PushCalleeSaveRegistersAndAdjustStack(BlockOfCode& code) {
    ABI_PushRegistersAndAdjustStack(code, ABI_ALL_CALLEE_SAVE);
}

void ABI_PopCalleeSaveRegistersAndAdjustStack(BlockOfCode& code) {
    ABI_PopRegistersAndAdjustStack(code, ABI_ALL_CALLEE_SAVE);
}

void ABI_PushCallerSaveRegistersAndAdjustStack(BlockOfCode& code) {
    ABI_PushRegistersAndAdjustStack(code, ABI_ALL_CALLER_SAVE);
}

void ABI_PopCallerSaveRegistersAndAdjustStack(BlockOfCode& code) {
    ABI_PopRegistersAndAdjustStack(code, ABI_ALL_CALLER_SAVE);
}

void ABI_PushCallerSaveRegistersAndAdjustStackExcept(BlockOfCode& code, HostLoc exception) {
    std::vector<HostLoc> regs;
    std::remove_copy(ABI_ALL_CALLER_SAVE.begin(), ABI_ALL_CALLER_SAVE.end(), std::back_inserter(regs), exception);
    ABI_PushRegistersAndAdjustStack(code, regs);
}

void ABI_PopCallerSaveRegistersAndAdjustStackExcept(BlockOfCode& code, HostLoc exception) {
    std::vector<HostLoc> regs;
    std::remove_copy(ABI_ALL_CALLER_SAVE.begin(), ABI_ALL_CALLER_SAVE.end(), std::back_inserter(regs), exception);
    ABI_PopRegistersAndAdjustStack(code, regs);
}

} // namespace Dynarmic::BackendX64
