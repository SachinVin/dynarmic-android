/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include "backend/A64/exception_handler.h"

namespace Dynarmic::BackendA64 {

struct ExceptionHandler::Impl final {
};

ExceptionHandler::ExceptionHandler() = default;
ExceptionHandler::~ExceptionHandler() = default;

void ExceptionHandler::Register(BlockOfCode&, std::function<void(CodePtr)>) {
    // Do nothing
}

bool ExceptionHandler::SupportsFastmem() const {
    return false;
}

} // namespace Dynarmic::BackendA64
