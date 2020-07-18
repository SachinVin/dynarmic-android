/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * SPDX-License-Identifier: 0BSD
 */

#pragma once

#include <utility>

#include "common/common_types.h"
#include "frontend/A32/location_descriptor.h"
#include "frontend/ir/ir_emitter.h"
#include "frontend/ir/value.h"

namespace Dynarmic::A32 {

enum class CoprocReg;
enum class Exception;
enum class ExtReg;
enum class Reg;

/**
 * Convenience class to construct a basic block of the intermediate representation.
 * `block` is the resulting block.
 * The user of this class updates `current_location` as appropriate.
 */
class IREmitter : public IR::IREmitter {
public:
    explicit IREmitter(IR::Block& block, LocationDescriptor descriptor) : IR::IREmitter(block), current_location(descriptor) {}

    LocationDescriptor current_location;

    u32 PC() const;
    u32 AlignPC(size_t alignment) const;

    IR::U32 GetRegister(Reg source_reg);
    IR::U32U64 GetExtendedRegister(ExtReg source_reg);
    IR::U128 GetVector(ExtReg source_reg);
    void SetRegister(Reg dest_reg, const IR::U32& value);
    void SetExtendedRegister(ExtReg dest_reg, const IR::U32U64& value);
    void SetVector(ExtReg dest_reg, const IR::U128& value);

    void ALUWritePC(const IR::U32& value);
    void BranchWritePC(const IR::U32& value);
    void BXWritePC(const IR::U32& value);
    void LoadWritePC(const IR::U32& value);

    void CallSupervisor(const IR::U32& value);
    void ExceptionRaised(Exception exception);

    IR::U32 GetCpsr();
    void SetCpsr(const IR::U32& value);
    void SetCpsrNZCV(const IR::U32& value);
    void SetCpsrNZCV(const IR::NZCV& value);
    void SetCpsrNZCVQ(const IR::U32& value);
    void SetCheckBit(const IR::U1& value);
    IR::U1 GetCFlag();
    void SetNFlag(const IR::U1& value);
    void SetZFlag(const IR::U1& value);
    void SetCFlag(const IR::U1& value);
    void SetVFlag(const IR::U1& value);
    void OrQFlag(const IR::U1& value);
    IR::U32 GetGEFlags();
    void SetGEFlags(const IR::U32& value);
    void SetGEFlagsCompressed(const IR::U32& value);

    void DataSynchronizationBarrier();
    void DataMemoryBarrier();
    void InstructionSynchronizationBarrier();

    IR::U32 GetFpscr();
    void SetFpscr(const IR::U32& new_fpscr);
    IR::U32 GetFpscrNZCV();
    void SetFpscrNZCV(const IR::NZCV& new_fpscr_nzcv);

    void ClearExclusive();
    IR::UAny ReadMemory(size_t bitsize, const IR::U32& vaddr);
    IR::U8 ReadMemory8(const IR::U32& vaddr);
    IR::U16 ReadMemory16(const IR::U32& vaddr);
    IR::U32 ReadMemory32(const IR::U32& vaddr);
    IR::U64 ReadMemory64(const IR::U32& vaddr);
    IR::U8 ExclusiveReadMemory8(const IR::U32& vaddr);
    IR::U16 ExclusiveReadMemory16(const IR::U32& vaddr);
    IR::U32 ExclusiveReadMemory32(const IR::U32& vaddr);
    std::pair<IR::U32, IR::U32> ExclusiveReadMemory64(const IR::U32& vaddr);
    void WriteMemory(size_t bitsize, const IR::U32& vaddr, const IR::UAny& value);
    void WriteMemory8(const IR::U32& vaddr, const IR::U8& value);
    void WriteMemory16(const IR::U32& vaddr, const IR::U16& value);
    void WriteMemory32(const IR::U32& vaddr, const IR::U32& value);
    void WriteMemory64(const IR::U32& vaddr, const IR::U64& value);
    IR::U32 ExclusiveWriteMemory8(const IR::U32& vaddr, const IR::U8& value);
    IR::U32 ExclusiveWriteMemory16(const IR::U32& vaddr, const IR::U16& value);
    IR::U32 ExclusiveWriteMemory32(const IR::U32& vaddr, const IR::U32& value);
    IR::U32 ExclusiveWriteMemory64(const IR::U32& vaddr, const IR::U32& value_lo, const IR::U32& value_hi);

    void CoprocInternalOperation(size_t coproc_no, bool two, size_t opc1, CoprocReg CRd, CoprocReg CRn, CoprocReg CRm, size_t opc2);
    void CoprocSendOneWord(size_t coproc_no, bool two, size_t opc1, CoprocReg CRn, CoprocReg CRm, size_t opc2, const IR::U32& word);
    void CoprocSendTwoWords(size_t coproc_no, bool two, size_t opc, CoprocReg CRm, const IR::U32& word1, const IR::U32& word2);
    IR::U32 CoprocGetOneWord(size_t coproc_no, bool two, size_t opc1, CoprocReg CRn, CoprocReg CRm, size_t opc2);
    IR::U64 CoprocGetTwoWords(size_t coproc_no, bool two, size_t opc, CoprocReg CRm);
    void CoprocLoadWords(size_t coproc_no, bool two, bool long_transfer, CoprocReg CRd, const IR::U32& address, bool has_option, u8 option);
    void CoprocStoreWords(size_t coproc_no, bool two, bool long_transfer, CoprocReg CRd, const IR::U32& address, bool has_option, u8 option);
};

} // namespace Dynarmic::A32
