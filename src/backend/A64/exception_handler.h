/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#pragma once

#include <array>
#include <memory>
#include <functional>

#include "backend/A64/a32_jitstate.h"
#include "common/common_types.h"

namespace Dynarmic::BackendA64 {

class BlockOfCode;

struct A64State {
    std::array<u64, 32> X;
    std::array<std::array<u64, 2>, 16> Q;
};
static_assert(sizeof(A64State) == sizeof(A64State::X) + sizeof(A64State::Q));

class ExceptionHandler final {
public:
    ExceptionHandler();
    ~ExceptionHandler();

    void Register(BlockOfCode& code, std::function<void(CodePtr)> segv_callback = nullptr);

    bool SupportsFastmem() const;
private:
    struct Impl;
    std::unique_ptr<Impl> impl;
};

} // namespace Dynarmic::BackendA64
