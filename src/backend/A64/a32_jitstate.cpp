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
 * N        bit 31       Negative flag
 * Z        bit 30       Zero flag
 * C        bit 29       Carry flag
 * V        bit 28       oVerflow flag
 * Q        bit 27       Saturation flag
 * IT[1:0]  bits 25-26   If-Then execution state (lower 2 bits)
 * J        bit 24       Jazelle instruction set flag
 * GE       bits 16-19   Greater than or Equal flags
 * IT[7:2]  bits 10-15   If-Then execution state (upper 6 bits)
 * E        bit 9        Data Endianness flag
 * A        bit 8        Disable imprecise Aborts
 * I        bit 7        Disable IRQ interrupts
 * F        bit 6        Disable FIQ interrupts
 * T        bit 5        Thumb instruction set flag
 * M        bits 0-4     Processor Mode bits
 *
 * A64 flags
 * -------------------
 * N    bit 31       Negative flag
 * Z    bit 30       Zero flag
 * C    bit 29       Carry flag
 * V    bit 28       oVerflow flag
 */

u32 A32JitState::Cpsr() const {
    DEBUG_ASSERT((cpsr_nzcv & ~0xF0000000) == 0);
    DEBUG_ASSERT((cpsr_q & ~1) == 0);
    DEBUG_ASSERT((cpsr_jaifm & ~0x010001DF) == 0);

    u32 cpsr = 0;

    // NZCV flags
    cpsr |= cpsr_nzcv;
    // Q flag
    cpsr |= cpsr_q ? 1 << 27 : 0;
    // GE flags
    cpsr |= Common::Bit<31>(cpsr_ge) ? 1 << 19 : 0;
    cpsr |= Common::Bit<23>(cpsr_ge) ? 1 << 18 : 0;
    cpsr |= Common::Bit<15>(cpsr_ge) ? 1 << 17 : 0;
    cpsr |= Common::Bit<7>(cpsr_ge) ? 1 << 16 : 0;
    // E flag, T flag
    cpsr |= Common::Bit<1>(upper_location_descriptor) ? 1 << 9 : 0;
    cpsr |= Common::Bit<0>(upper_location_descriptor) ? 1 << 5 : 0;
    // IT state
    cpsr |= static_cast<u32>(upper_location_descriptor & 0b11111100'00000000);
    cpsr |= static_cast<u32>(upper_location_descriptor & 0b00000011'00000000) << 17;
    // Other flags
    cpsr |= cpsr_jaifm;

    return cpsr;
}

void A32JitState::SetCpsr(u32 cpsr) {
    // NZCV flags
    cpsr_nzcv = cpsr & 0xF0000000;
    // Q flag
    cpsr_q = Common::Bit<27>(cpsr) ? 1 : 0;
    // GE flags
    cpsr_ge = 0;
    cpsr_ge |= Common::Bit<19>(cpsr) ? 0xFF000000 : 0;
    cpsr_ge |= Common::Bit<18>(cpsr) ? 0x00FF0000 : 0;
    cpsr_ge |= Common::Bit<17>(cpsr) ? 0x0000FF00 : 0;
    cpsr_ge |= Common::Bit<16>(cpsr) ? 0x000000FF : 0;

    upper_location_descriptor &= 0xFFFF0000;
    // E flag, T flag
    upper_location_descriptor |= Common::Bit<9>(cpsr) ? 2 : 0;
    upper_location_descriptor |= Common::Bit<5>(cpsr) ? 1 : 0;
    // IT state
    upper_location_descriptor |= (cpsr >>  0) & 0b11111100'00000000;
    upper_location_descriptor |= (cpsr >> 17) & 0b00000011'00000000;

    // Other flags
    cpsr_jaifm = cpsr & 0x010001DF;
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
 * AHP      bit 26      Alternate half-precision
 * DN       bit 25      Default NaN
 * FZ       bit 24      Flush to Zero
 * RMode    bits 22-23  Round to {0 = Nearest, 1 = Positive, 2 = Negative, 3 = Zero}
 * Stride   bits 20-21  Vector stride
 * Len      bits 16-18  Vector length
 */

// NZCV; QC (ASIMD only), AHP; DN, FZ, RMode, Stride; SBZP; Len; trap enables; cumulative bits
constexpr u32 FPSCR_MODE_MASK = A32::LocationDescriptor::FPSCR_MODE_MASK;
constexpr u32 FPSCR_NZCV_MASK = 0xF0000000;

u32 A32JitState::Fpscr() const {
    DEBUG_ASSERT((fpsr_nzcv & ~FPSCR_NZCV_MASK) == 0);

    const u32 fpcr_mode = static_cast<u32>(upper_location_descriptor) & FPSCR_MODE_MASK;

    u32 FPSCR = fpcr_mode | fpsr_nzcv;
    FPSCR |= (guest_fpsr & 0x1F);
    FPSCR |= fpsr_exc;

    return FPSCR;
}

void A32JitState::SetFpscr(u32 FPSCR) {
    // Ensure that only upper half of upper_location_descriptor is used for FPSCR bits.
    static_assert((FPSCR_MODE_MASK & 0xFFFF0000) == FPSCR_MODE_MASK);

    upper_location_descriptor &= 0x0000FFFF;
    upper_location_descriptor |= FPSCR & FPSCR_MODE_MASK;

    fpsr_nzcv = FPSCR & FPSCR_NZCV_MASK;
    guest_fpcr = 0;
    guest_fpsr = 0;

    // Cumulative flags IDC, IOC, IXC, UFC, OFC, DZC
    fpsr_exc = FPSCR & 0x9F;

    // Mode Bits
    guest_fpcr |= FPSCR & 0x07C09F00;

    // Exceptions
    guest_fpsr |= FPSCR & 0x9F;
}

} // namespace Dynarmic::BackendA64
