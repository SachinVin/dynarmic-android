/* This file is part of the dynarmic project.
 * Copyright (c) 2018 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#pragma once

#include <functional>
#include <vector>

#include "backend/A64/emitter/a64_emitter.h"
#include "common/common_types.h"

namespace Dynarmic::BackendA64 {

using RegList = std::vector<Arm64Gen::ARM64Reg>;

class BlockOfCode;

class Callback {
public:
    virtual ~Callback();

    virtual void EmitCall(BlockOfCode& code, std::function<void(RegList)> fn = [](RegList) {}) const = 0;
    virtual void EmitCallWithReturnPointer(BlockOfCode& code, std::function<void(Arm64Gen::ARM64Reg, RegList)> fn) const = 0;
};

class SimpleCallback final : public Callback {
public:
    template <typename Function>
    SimpleCallback(Function fn) : fn(reinterpret_cast<void (*)()>(fn)) {}

    void EmitCall(BlockOfCode& code, std::function<void(RegList)> fn = [](RegList) {}) const override;
    void EmitCallWithReturnPointer(BlockOfCode& code, std::function<void(Arm64Gen::ARM64Reg, RegList)> fn) const override;

private:
    void (*fn)();
};

class ArgCallback final : public Callback {
public:
    template <typename Function>
    ArgCallback(Function fn, u64 arg) : fn(reinterpret_cast<void (*)()>(fn)), arg(arg) {}

    void EmitCall(BlockOfCode& code, std::function<void(RegList)> fn = [](RegList) {}) const override;
    void EmitCallWithReturnPointer(BlockOfCode& code, std::function<void(Arm64Gen::ARM64Reg, RegList)> fn) const override;

private:
    void (*fn)();
    u64 arg;
};

} // namespace Dynarmic::BackendA64
