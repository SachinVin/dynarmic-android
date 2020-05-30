/* This file is part of the dynarmic project.
 * Copyright (c) 2019 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

// Copyright 2008 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include <mutex>
#include <vector>

#include <csignal>
#include <ucontext.h>

#include "backend/A64/a32_jitstate.h"
#include "backend/A64/block_of_code.h"
#include "backend/A64/exception_handler.h"
#include "common/assert.h"
#include "common/cast_util.h"
#include "common/common_types.h"

namespace Dynarmic::BackendA64 {

namespace {

struct CodeBlockInfo {
    BlockOfCode* block;
    std::function<void(CodePtr)> callback;
};

class SigHandler {
public:
    SigHandler();

    ~SigHandler();

    void AddCodeBlock(CodeBlockInfo info);

    void RemoveCodeBlock(CodePtr PC);

private:
    auto FindCodeBlockInfo(CodePtr PC) {
        return std::find_if(code_block_infos.begin(), code_block_infos.end(),
                            [&](const CodeBlockInfo& x) { return x.block->GetRegion() <= PC && x.block->GetRegion() + x.block->GetRegionSize() > PC; });
    }

    std::vector<CodeBlockInfo> code_block_infos;
    std::mutex code_block_infos_mutex;

    struct sigaction old_sa_segv;
    struct sigaction old_sa_bus;

    static void SigAction(int sig, siginfo_t* info, void* raw_context);
};

SigHandler sig_handler;

SigHandler::SigHandler() {
    // Method below from dolphin.

    constexpr std::size_t signal_stack_size =
        static_cast<std::size_t>(std::max(SIGSTKSZ, 2 * 1024 * 1024));

    stack_t signal_stack;
    signal_stack.ss_sp = malloc(signal_stack_size);
    signal_stack.ss_size = signal_stack_size;
    signal_stack.ss_flags = 0;
    ASSERT_MSG(sigaltstack(&signal_stack, nullptr) == 0,
               "dynarmic: POSIX SigHandler: init failure at sigaltstack");

    struct sigaction sa;
    sa.sa_handler = nullptr;
    sa.sa_sigaction = &SigHandler::SigAction;
    sa.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_RESTART;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGSEGV, &sa, &old_sa_segv);
}

SigHandler::~SigHandler() {
    // No cleanup required.
}

void SigHandler::AddCodeBlock(CodeBlockInfo cb) {
    std::lock_guard<std::mutex> guard(code_block_infos_mutex);
    ASSERT(FindCodeBlockInfo(cb.block->GetRegion()) == code_block_infos.end());
    code_block_infos.push_back(std::move(cb));
}

void SigHandler::RemoveCodeBlock(CodePtr PC) {
    std::lock_guard<std::mutex> guard(code_block_infos_mutex);
    const auto iter = FindCodeBlockInfo(PC);
    ASSERT(iter != code_block_infos.end());
    code_block_infos.erase(iter);
}

void SigHandler::SigAction(int sig, siginfo_t* info, void* raw_context) {
    ASSERT(sig == SIGSEGV || sig == SIGBUS);

    std::lock_guard<std::mutex> guard(sig_handler.code_block_infos_mutex);
    auto PC = reinterpret_cast<CodePtr>(((ucontext_t*)raw_context)->uc_mcontext.pc);
    const auto iter = sig_handler.FindCodeBlockInfo(PC);
    if (iter != sig_handler.code_block_infos.end()) {
        iter->callback(PC);
        return;
    }

    fmt::print(
        stderr,
        "dynarmic: POSIX SigHandler: Exception was not in registered code blocks (PC {})\n",
        PC);

    struct sigaction* retry_sa =
        sig == SIGSEGV ? &sig_handler.old_sa_segv : &sig_handler.old_sa_bus;
    if (retry_sa->sa_flags & SA_SIGINFO) {
        retry_sa->sa_sigaction(sig, info, raw_context);
        return;
    }
    if (retry_sa->sa_handler == SIG_DFL) {
        signal(sig, SIG_DFL);
        return;
    }
    if (retry_sa->sa_handler == SIG_IGN) {
        return;
    }
    retry_sa->sa_handler(sig);
}

} // anonymous namespace

struct ExceptionHandler::Impl final {
    Impl(BlockOfCode& code, std::function<void(CodePtr)> cb) {
        code_begin = code.GetRegion();
        sig_handler.AddCodeBlock({&code, std::move(cb)});
    }

    ~Impl() {
        sig_handler.RemoveCodeBlock(code_begin);
    }

private:
    CodePtr code_begin;
};

ExceptionHandler::ExceptionHandler() = default;

ExceptionHandler::~ExceptionHandler() = default;

void ExceptionHandler::Register(BlockOfCode& code, std::function<void(CodePtr)> cb) {
    if (cb)
        impl = std::make_unique<Impl>(code, std::move(cb));
}

bool ExceptionHandler::SupportsFastmem() const {
    return static_cast<bool>(impl);
}

} // namespace Dynarmic::BackendA64
