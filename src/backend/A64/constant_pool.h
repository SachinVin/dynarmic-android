/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#pragma once

#include <map>

#include "common/common_types.h"

namespace Dynarmic::BackendA64 {

class BlockOfCode;

/// ConstantPool allocates a block of memory from BlockOfCode.
/// It places constants into this block of memory, returning the address
/// of the memory location where the constant is placed. If the constant
/// already exists, its memory location is reused.
class ConstantPool final {
public:
    ConstantPool(BlockOfCode& code);

    void EmitPatchLDR(Arm64Gen::ARM64Reg Rt, u64 lower, u64 upper = 0);

    void PatchPool();

    void Clear();

private:
    static constexpr size_t align_size = 16; // bytes

    std::map<std::tuple<u64, u64>, void*> constant_info;

    BlockOfCode& code;

    struct PatchInfo {
        const void* ptr;
        Arm64Gen::ARM64Reg Rt;
        std::tuple<u64, u64> constant;
    };

    std::vector<PatchInfo> patch_info;
};

} // namespace Dynarmic::BackendA64
