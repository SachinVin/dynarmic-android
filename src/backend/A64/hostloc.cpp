/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include "backend/A64/hostloc.h"

namespace Dynarmic::BackendA64 {

Arm64Gen::ARM64Reg HostLocToReg64(HostLoc loc) {
    ASSERT(HostLocIsGPR(loc));
    return static_cast<Arm64Gen::ARM64Reg>(static_cast<int>(Arm64Gen::X0) + static_cast<int>(loc));
}

Arm64Gen::ARM64Reg HostLocToFpr(HostLoc loc) {
    ASSERT(HostLocIsFPR(loc));
    return EncodeRegToQuad(static_cast<Arm64Gen::ARM64Reg>(static_cast<int>(loc) - static_cast<int>(HostLoc::Q0)));
}

} // namespace Dynarmic::BackendX64
