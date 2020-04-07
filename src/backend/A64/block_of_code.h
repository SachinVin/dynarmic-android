/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#pragma once

#include <array>
#include <memory>
#include <type_traits>

#include "backend/A64/callback.h"
#include "backend/A64/constant_pool.h"
#include "backend/A64/jitstate_info.h"
#include "backend/A64/emitter/a64_emitter.h"
#include "common/common_types.h"

namespace Dynarmic::BackendA64 {

using CodePtr = const void*;

struct RunCodeCallbacks {
    std::unique_ptr<Callback> LookupBlock;
    std::unique_ptr<Callback> AddTicks;
    std::unique_ptr<Callback> GetTicksRemaining;
    u64 value_in_X27;
};

class BlockOfCode final : public Arm64Gen::ARM64CodeBlock {
public:
    BlockOfCode(RunCodeCallbacks cb, JitStateInfo jsi);
    BlockOfCode(const BlockOfCode&) = delete;


    /// Call when external emitters have finished emitting their preludes.
    void PreludeComplete();

    /// Change permissions to RW. This is required to support systems with W^X enforced.
    void EnableWriting();
    /// Change permissions to RX. This is required to support systems with W^X enforced.
    void DisableWriting();

    /// Clears this block of code and resets code pointer to beginning.
    void ClearCache();
    /// Calculates how much space is remaining to use. This is the minimum of near code and far code.
    size_t SpaceRemaining() const;

    /// Runs emulated code from code_ptr.
    void RunCode(void* jit_state, CodePtr code_ptr) const;
    /// Runs emulated code from code_ptr for a single cycle.
    void StepCode(void* jit_state, CodePtr code_ptr) const;
    /// Code emitter: Returns to dispatcher
    void ReturnFromRunCode(bool fpscr_already_exited = false);
    /// Code emitter: Returns to dispatcher, forces return to host
    void ForceReturnFromRunCode(bool fpscr_already_exited = false);
    /// Code emitter: Makes guest FPSR and FPCR the current FPSR and FPCR
    void SwitchFpscrOnEntry();
    /// Code emitter: Makes saved host FPCR the current FPCR
    void SwitchFpscrOnExit();
    /// Code emitter: Updates cycles remaining my calling cb.AddTicks and cb.GetTicksRemaining
    /// @note this clobbers ABI caller-save registers
    void UpdateTicks();
    /// Code emitter: Performs a block lookup based on current state
    /// @note this clobbers ABI caller-save registers
    void LookupBlock();

    u64 MConst(u64 lower, u64 upper = 0);

    void EmitPatchLDR(Arm64Gen::ARM64Reg Rt, u64 lower, u64 upper = 0);

    void PatchConstPool();

    /// Far code sits far away from the near code. Execution remains primarily in near code.
    /// "Cold" / Rarely executed instructions sit in far code, so the CPU doesn't fetch them unless necessary.
    void SwitchToFarCode();
    void SwitchToNearCode();

    CodePtr GetCodeBegin() const;
    u8* GetRegion() const;
    std::size_t GetRegionSize() const;

    const void* GetReturnFromRunCodeAddress() const {
        return return_from_run_code[0];
    }

    const void* GetForceReturnFromRunCodeAddress() const {
        return return_from_run_code[FORCE_RETURN];
    }

    /// Allocate memory of `size` bytes from the same block of memory the code is in.
    /// This is useful for objects that need to be placed close to or within code.
    /// The lifetime of this memory is the same as the code around it.
    void* AllocateFromCodeSpace(size_t size);

    void SetCodePtr(CodePtr code_ptr);
    void EnsurePatchLocationSize(CodePtr begin, size_t size);

    Arm64Gen::ARM64FloatEmitter fp_emitter;

    // ABI registers

    static const Arm64Gen::ARM64Reg ABI_RETURN;
    static const Arm64Gen::ARM64Reg ABI_RETURN2;
    static const Arm64Gen::ARM64Reg ABI_PARAM1;
    static const Arm64Gen::ARM64Reg ABI_PARAM2;
    static const Arm64Gen::ARM64Reg ABI_PARAM3;
    static const Arm64Gen::ARM64Reg ABI_PARAM4;
    static const Arm64Gen::ARM64Reg ABI_PARAM5;
    static const Arm64Gen::ARM64Reg ABI_PARAM6;
    static const Arm64Gen::ARM64Reg ABI_PARAM7;
    static const Arm64Gen::ARM64Reg ABI_PARAM8;

    static const Arm64Gen::ARM64Reg ABI_SCRATCH1;

    static const std::array<Arm64Gen::ARM64Reg, 8> ABI_PARAMS;

    // bool DoesCpuSupport(Xbyak::util::Cpu::Type type) const;

    JitStateInfo GetJitStateInfo() const { return jsi; }

private:
    RunCodeCallbacks cb;
    JitStateInfo jsi;

    bool prelude_complete = false;
    CodePtr near_code_begin;
    CodePtr far_code_begin;

    ConstantPool constant_pool;

    bool in_far_code = false;
    CodePtr near_code_ptr;
    CodePtr far_code_ptr;

    using RunCodeFuncType = void(*)(void*, CodePtr);
    RunCodeFuncType run_code = nullptr;
    RunCodeFuncType step_code = nullptr;
    static constexpr size_t FPSCR_ALREADY_EXITED = 1 << 0;
    static constexpr size_t FORCE_RETURN = 1 << 1;
    std::array<const void*, 4> return_from_run_code;
    void GenRunCode();

    //Xbyak::util::Cpu cpu_info;
};

} // namespace Dynarmic::BackendA64
