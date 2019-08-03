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

void* ConstantPool::GetConstant(u64 lower, u64 upper) {
    const auto constant = std::make_tuple(lower, upper);
    auto iter = constant_info.find(constant);
    if (iter == constant_info.end()) {
        ASSERT(static_cast<size_t>(current_pool_ptr - pool_begin) < pool_size);
        std::memcpy(current_pool_ptr, &lower, sizeof(u64));
        std::memcpy(current_pool_ptr + sizeof(u64), &upper, sizeof(u64));
        iter = constant_info.emplace(constant, current_pool_ptr).first;
        current_pool_ptr += align_size;
    }
    return iter->second;
}

} // namespace Dynarmic::BackendX64
