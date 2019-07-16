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

ConstantPool::ConstantPool(BlockOfCode& code, size_t size) : code(code), pool_size(size) {}

void ConstantPool::AllocatePool() {
    code.BRK(0);
    pool_begin = const_cast<u8*>(code.AlignCode16());
    code.AllocateFromCodeSpace(pool_size);
    current_pool_ptr = pool_begin;
    ASSERT(code.GetCodePtr() - pool_begin == static_cast<u32>(pool_size));
}

u64 ConstantPool::GetConstant(u64 lower, u64 upper) {
    const auto constant = std::make_tuple(lower, upper);
    auto iter = constant_info.find(constant);
    if (iter == constant_info.end()) {
        ASSERT(static_cast<size_t>(current_pool_ptr - pool_begin) < pool_size);
        std::memcpy(current_pool_ptr, &lower, sizeof(u64));
        std::memcpy(current_pool_ptr + sizeof(u64), &upper, sizeof(u64));
        iter = constant_info.emplace(constant, current_pool_ptr).first;
        current_pool_ptr += align_size;
    }
    return reinterpret_cast<u64>(iter->second) - reinterpret_cast<u64>(code.GetCodePtr());
}

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

    code.LDR(Rt, offset);
}

void ConstantPool::PatchPool() {

    u8* pool_ptr = const_cast<u8*>(code.GetCodePtr());
    for (PatchInfo patch : patch_info) {
        std::memcpy(pool_ptr, &std::get<0>(patch.constant), sizeof(u64));
        std::memcpy(pool_ptr + sizeof(u64), &std::get<1>(patch.constant), sizeof(u64));
        constant_info.emplace(patch.constant, pool_ptr);

        code.SetCodePtr(patch.ptr);
        size_t offset = reinterpret_cast<size_t>(pool_ptr) - reinterpret_cast<size_t>(code.GetCodePtr());
        code.LDR(patch.Rt, offset);

        pool_ptr += align_size;
    }
    patch_info.clear();
    code.SetCodePtr(pool_ptr);
}

void  ConstantPool::Clear() {
    constant_info.clear();
    patch_info.clear();
}

} // namespace Dynarmic::BackendX64
