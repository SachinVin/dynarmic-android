/* This file is part of the dynarmic project.
 * Copyright (c) 2018 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <array>

#include <boost/variant/get.hpp>

#include "common/assert.h"
#include "common/common_types.h"
#include "dynarmic/A32/config.h"
#include "frontend/A32/location_descriptor.h"
#include "frontend/A32/translate/translate.h"
#include "frontend/ir/basic_block.h"
#include "ir_opt/passes.h"

namespace Dynarmic::Optimization {

void A32MergeInterpretBlocksPass(IR::Block& block, A32::UserCallbacks* cb) {
    const auto is_interpret_instruction = [cb](A32::LocationDescriptor location) {
        const u32 instruction = cb->MemoryReadCode(location.PC());

        IR::Block new_block{location};
        A32::TranslateSingleInstruction(new_block, location, instruction);

        if (!new_block.Instructions().empty())
            return false;

        const IR::Terminal terminal = new_block.GetTerminal();
        if (auto term = boost::get<IR::Term::Interpret>(&terminal)) {
            return term->next == location;
        }

        return false;
    };

    IR::Terminal terminal = block.GetTerminal();
    auto term = boost::get<IR::Term::Interpret>(&terminal);
    if (!term)
        return;

    A32::LocationDescriptor location{term->next};
    size_t num_instructions = 1;

    while (is_interpret_instruction(location.AdvancePC(static_cast<int>(num_instructions * 4)))) {
        num_instructions++;
    }

    term->num_instructions = num_instructions;
    block.ReplaceTerminal(terminal);
    block.CycleCount() += num_instructions - 1;
}

} // namespace Dynarmic::Optimization
