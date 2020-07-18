/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include "backend/A64/a32_jitstate.h"
#include "backend/A64/block_of_code.h"
#include "common/assert.h"
#include "common/bit_util.h"
#include "common/common_types.h"
#include "frontend/A32/location_descriptor.h"

namespace Dynarmic::BackendA64 {

/**
 * CPSR Bits
 * =========
 *
 * ARM CPSR flags
 * --------------
 * N    bit 31       Negative flag
 * Z    bit 30       Zero flag
 * C    bit 29       Carry flag
 * V    bit 28       oVerflow flag
 * Q    bit 27       Saturation flag
 * J    bit 24       Jazelle instruction set flag
 * GE   bits 16-19   Greater than or Equal flags
 * E    bit 9        Data Endianness flag
 * A    bit 8        Disable imprecise Aborts
 * I    bit 7        Disable IRQ interrupts
 * F    bit 6        Disable FIQ interrupts
 * T    bit 5        Thumb instruction set flag
 * M    bits 0-4     Processor Mode bits
 *
 * A64 flags
 * -------------------
 * N    bit 31       Negative flag
 * Z    bit 30       Zero flag
 * C    bit 29       Carry flag
 * V    bit 28       oVerflow flag
 */

u32 A32JitState::Cpsr() const {
    ASSERT((CPSR_nzcv & ~0xF0000000) == 0);
    ASSERT((CPSR_q & ~1) == 0);
    ASSERT((CPSR_et & ~3) == 0);
    ASSERT((CPSR_jaifm & ~0x010001DF) == 0);

    u32 cpsr = 0;

    // NZCV flags
    cpsr |= CPSR_nzcv;
    // Q flag
    cpsr |= CPSR_q ? 1 << 27 : 0;
    // GE flags
    cpsr |= Common::Bit<31>(CPSR_ge) ? 1 << 19 : 0;
    cpsr |= Common::Bit<23>(CPSR_ge) ? 1 << 18 : 0;
    cpsr |= Common::Bit<15>(CPSR_ge) ? 1 << 17 : 0;
    cpsr |= Common::Bit<7>(CPSR_ge) ? 1 << 16 : 0;
    // E flag, T flag
    cpsr |= Common::Bit<1>(CPSR_et) ? 1 << 9 : 0;
    cpsr |= Common::Bit<0>(CPSR_et) ? 1 << 5 : 0;
    // Other flags
    cpsr |= CPSR_jaifm;

    return cpsr;
}

void A32JitState::SetCpsr(u32 cpsr) {
    // NZCV flags
    CPSR_nzcv = cpsr & 0xF0000000;
    // Q flag
    CPSR_q = Common::Bit<27>(cpsr) ? 1 : 0;
    // GE flags
    CPSR_ge = 0;
    CPSR_ge |= Common::Bit<19>(cpsr) ? 0xFF000000 : 0;
    CPSR_ge |= Common::Bit<18>(cpsr) ? 0x00FF0000 : 0;
    CPSR_ge |= Common::Bit<17>(cpsr) ? 0x0000FF00 : 0;
    CPSR_ge |= Common::Bit<16>(cpsr) ? 0x000000FF : 0;
    // E flag, T flag
    CPSR_et = 0;
    CPSR_et |= Common::Bit<9>(cpsr) ? 2 : 0;
    CPSR_et |= Common::Bit<5>(cpsr) ? 1 : 0;
    // Other flags
    CPSR_jaifm = cpsr & 0x07F0FDDF;
}

void A32JitState::ResetRSB() {
    rsb_location_descriptors.fill(0xFFFFFFFFFFFFFFFFull);
    rsb_codeptrs.fill(0);
}

/**
 * FPSCR
 * =========================
 *
 * VFP FPSCR cumulative exception bits
 * -----------------------------------
 * IDC  bit 7   Input Denormal cumulative exception bit       // Only ever set when FPSCR.FTZ = 1
 * IXC  bit 4   Inexact cumulative exception bit
 * UFC  bit 3   Underflow cumulative exception bit
 * OFC  bit 2   Overflow cumulative exception bit
 * DZC  bit 1   Division by Zero cumulative exception bit
 * IOC  bit 0   Invalid Operation cumulative exception bit
 *
 * VFP FPSCR exception trap enables
 * --------------------------------
 * IDE  bit 15  Input Denormal exception trap enable
 * IXE  bit 12  Inexact exception trap enable
 * UFE  bit 11  Underflow exception trap enable
 * OFE  bit 10  Overflow exception trap enable
 * DZE  bit 9   Division by Zero exception trap enable
 * IOE  bit 8   Invalid Operation exception trap enable
 *
 * VFP FPSCR mode bits
 * -------------------
 * DN   bit 25  Default NaN
 * FZ   bit 24  Flush to Zero
 * RMode    bits 22-23  Round to {0 = Nearest, 1 = Positive, 2 = Negative, 3 = Zero}
 * Stride   bits 20-21  Vector stride
 * Len  bits 16-18  Vector length
 */

// NZCV; QC (ASMID only), AHP; DN, FZ, RMode, Stride; SBZP; Len; trap enables; cumulative bits
constexpr u32 FPSCR_MODE_MASK = A32::LocationDescriptor::FPSCR_MODE_MASK;
constexpr u32 FPSCR_NZCV_MASK = 0xF0000000;

u32 A32JitState::Fpscr() const {
    ASSERT((FPSCR_mode & ~FPSCR_MODE_MASK) == 0);
    ASSERT((FPSCR_nzcv & ~FPSCR_NZCV_MASK) == 0);
    ASSERT((FPSCR_IDC & ~(1 << 7)) == 0);
    ASSERT((FPSCR_UFC & ~(1 << 3)) == 0);

    u32 FPSCR = FPSCR_mode | FPSCR_nzcv;
    FPSCR |= (guest_FPSR & 0x1F);
    FPSCR |= FPSCR_IDC;
    FPSCR |= FPSCR_UFC;
    FPSCR |= fpsr_exc;

    return FPSCR;
}

void A32JitState::SetFpscr(u32 FPSCR) {
    old_FPSCR = FPSCR;
    FPSCR_mode = FPSCR & FPSCR_MODE_MASK;
    FPSCR_nzcv = FPSCR & FPSCR_NZCV_MASK;
    guest_FPCR = 0;
    guest_FPSR = 0;
    // Cumulative flags IDC, IOC, IXC, UFC, OFC, DZC
    FPSCR_IDC = 0;
    FPSCR_UFC = 0;
    fpsr_exc = FPSCR & 0x9F;

    // Mode Bits
    guest_FPCR |= FPSCR & 0x07C09F00;

    // Exceptions
    guest_FPSR |= FPSCR & 0x9F;
}

u64 A32JitState::GetUniqueHash() const {
    return CPSR_et | FPSCR_mode | (static_cast<u64>(Reg[15]) << 32);
}

} // namespace Dynarmic::BackendX64
