package cpu

const (
	opAdd      = 1
	opMultiply = 2
	opHalt     = 99
)

type CPU struct {
	program, Memory []int
	pc              int
}

func NewCPU(program []int) *CPU {
	memory := make([]int, len(program))
	copy(memory, program)
	return &CPU{program, memory, 0}
}

func (c *CPU) SetInput(noun, verb int) {
	c.Memory[1] = noun
	c.Memory[2] = verb
}

func (c *CPU) Reset() {
	copy(c.Memory, c.program)
}

func (c *CPU) Run() {
	for op := c.Memory[c.pc]; op != opHalt; op = c.Memory[c.pc] {
		xAddr := c.Memory[c.pc+1]
		yAddr := c.Memory[c.pc+2]
		outAddr := c.Memory[c.pc+3]

		switch op {
		case opAdd:
			c.Memory[outAddr] = c.Memory[xAddr] + c.Memory[yAddr]
		case opMultiply:
			c.Memory[outAddr] = c.Memory[xAddr] * c.Memory[yAddr]
		}

		c.pc += 4
	}
}

func (c *CPU) Output() int {
	return c.Memory[0]
}
