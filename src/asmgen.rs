// Here we generate low-level intermediate code from the IR.
// This means erasing the distinction between locals and values,
// at a minimum, and probably other lowering.  Ideally we'd do
// regalloc after this phase, but probably using the same form.
// So here we would use a register form, and we would save
// and restore live registers around calls.

// Not sure

// Use triples:

enum Op {
    op2(op, rd, rs1, rs2),
    op2imm(op, rd, rs, imm),
    op1(op, rd, rs),
}


