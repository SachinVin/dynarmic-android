/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <cstring>

#include "backend/A64/block_of_code.h"
#include "backend/A64/constant_pool.h"
#include "common/assert.h"

namespace Dynarmic::BackendA64 {

ConstantPool::ConstantPool(BlockOfCode& code) : code(code) {}

void ConstantPool::EmitPatchLDR(Arm64Gen::ARM64Reg Rt, u64 lower, u64 upper) {
    const auto constant = std::make_tuple(lower, upper);
    auto iter = constant_info.find(constant);
    if (iter == constant_info.end()) {
        struct PatchInfo p = { code.GetCodePtr(), Rt, constant };
        patch_info.emplace_back(p);
        code.BRK(0);
        return;
    }

    const s32 offset = reinterpret_cast<size_t>(iter->second) - reinterpret_cast<size_t>(code.GetCodePtr());

    if (!(offset >= -0x40000 && offset <= 0x3FFFF)) {
        constant_info.erase(constant);
        struct PatchInfo p = { code.GetCodePtr(), Rt, constant };
        patch_info.emplace_back(p);
        code.BRK(0x42);
        return;
    }
    DEBUG_ASSERT((offset & 3) == 0);
    code.LDR(Rt, offset / 4);
}

void ConstantPool::PatchPool() {
    u8* pool_ptr = code.GetWritableCodePtr();
    for (PatchInfo patch : patch_info) {
        auto iter = constant_info.find(patch.constant);
        if (iter == constant_info.end()) {
            std::memcpy(pool_ptr, &std::get<0>(patch.constant), sizeof(u64));
            std::memcpy(pool_ptr + sizeof(u64), &std::get<1>(patch.constant), sizeof(u64));
            iter = constant_info.emplace(patch.constant, pool_ptr).first;
            pool_ptr += align_size;
        }
        code.SetCodePtr(patch.ptr);

        const s32 offset = reinterpret_cast<size_t>(iter->second) - reinterpret_cast<size_t>(code.GetCodePtr());
        DEBUG_ASSERT((offset & 3) == 0);
        code.LDR(patch.Rt, offset / 4);
    }
    patch_info.clear();
    code.SetCodePtr(pool_ptr);
}

void  ConstantPool::Clear() {
    constant_info.clear();
    patch_info.clear();
}

} // namespace Dynarmic::BackendA64
