package main

import (
	"bufio"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"math"
	"os"
)

func main() {
	ch1 := NewBufferHex("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
	println("> CH1-1: Convert hex to base64")
	fmt.Println(ch1.ToB64())

	ch2a := NewBufferHex("1c0111001f010100061a024b53535009181c")
	ch2b := NewBufferHex("686974207468652062756c6c277320657965")
	println("> CH1-2: Fixed XOR")
	fmt.Println(ch2a.Xor(ch2b).ToHex())

	ch3 := NewBufferHex("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
	println("> CH1-3: Convert hex to base64")
	fmt.Println(string(ch3.bruteXor()))

	ch4 := NewBufferHexFile("ch4-input.txt")
	println("> CH1-4: Detect single-character XOR")
	fmt.Println(ch4.mapBruteXor().bestCandidate())

	ch5 := NewBufferString("Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
	ch5Key := NewBufferString("ICE")
	println("> CH1-5: Implement repeating-key XOR")
	fmt.Println(ch5.Xor(ch5Key).toHex())

}

func (bsl BytesList) mapBruteXor() BytesList {
	ret := make(BytesList, len(bsl))
	for i := range bsl {
		ret[i] = bsl[i].bruteXor()
	}
	return ret
}

func (bsl BytesList) bestCandidate() Bytes {
	ret := bsl[0]
	scr := bsl[0].score()
	for i := 1; i < len(bsl); i++ {
		if bsl[i].score() > scr {
			ret = bsl[i]
			scr = bsl[i].score()
		}
	}
	return ret
}

/* Reimplement it all in a Buffer structure */
func NewBuffer(buf []byte) *Buffer {
	return &Buffer{buf: buf}
}

func NewBufferString(s string) *Buffer {
	return &Buffer{buf: []byte(s)}
}

func NewBufferHexFile(s string) []Buffer {
	file, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer file.Close()

	var lines []Buffer
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, NewBufferHex(scanner.Text()))
	}
	return lines
}

func NewBufferHex(s string) *Buffer {
	out, _ := hex.DecodeString(s)
	return &Buffer{buf: out}
}

func NewBufferBase64(s string) *Buffer {
	out, _ := base64.StdEncoding.DecodeString(s)
	return &Buffer{buf: out}
}

type Buffer struct {
	off int
	buf []byte
}

func (b *Buffer) ReadByteLoop() (c byte, err error) {
	if b.off >= len(b.buf) {
		b.off = 0
	}
	c = b.buf[b.off]
	b.off++
	return c, nil
}

func (b *Buffer) ReadByte() (c byte, err error) {
	if b.off >= len(b.buf) {
		return 0, errors.New("End of key")
	}
	c = b.buf[b.off]
	b.off++
	return c, nil
}

func (b *Buffer) ToHex() string {
	return hex.EncodeToString(b.buf)
}

func (b *Buffer) ToB64() string {
	return base64.StdEncoding.EncodeToString(b.buf)
}

func (b *Buffer) Score() (r float64) {
	for _, b := range b.buf {
		r += math.Log(scoreByte(b))
	}
	return
}

func (b *Buffer) Xor(a *Buffer) (ret *Buffer) {
	i := 0
	for {
		if k, err := a.ReadByte(); err == nil && i < len(b) {
			ret.buf = append(ret.buf, b[i]^k)
			i++
		} else {
			return
		}
	}
}

func (b *Buffer) BruteXor() *Buffer {
	ret := b.xorSingle(32)
	retScore := b.Score()
	var retCandidate Bytes

	for i := 33; i < 123; i++ {
		retCandidate = bs.xorSingle(byte(i))
		if retCandidate.score() > retScore {
			retScore = retCandidate.score()
			ret = retCandidate
		}
	}
	return ret
}

func minInt(a, b int) int {
	if a > b {
		return b
	} else {
		return a
	}
}

func scoreByte(a byte) float64 {
	scoreAZ := []float64{0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974, 0.00074}
	switch {
	case a == 32:
		return 0.14
	case a > 64 && a < 91:
		return scoreAZ[a-65]
	case a > 96 && a < 123:
		return scoreAZ[a-97]
	case a > 32 && a < 128:
		return 0.00085
	}
	return 0.00000001
}
