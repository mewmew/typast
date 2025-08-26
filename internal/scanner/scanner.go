package scanner

import "unicode/utf8"

const EOF = -1

type Scanner struct {
	str   string
	cur   int
	start int
}

func NewScanner(str string) *Scanner {
	return &Scanner{
		str: str,
	}
}

func (s *Scanner) Done() bool {
	// TODO: should be `if s.cur > len(s.str) {`?
	if s.cur >= len(s.str) {
		return true
	}
	return false
}

func (s *Scanner) EatIf(want rune) bool {
	c, n := s.next()
	if c == want {
		s.cur += n
		return true
	}
	return false
}

func (s *Scanner) EatUntil(stop rune) string {
	for {
		c, n := s.next()
		if c == stop {
			break
		}
		s.cur += n
	}
	return s.emit()
}

func (s *Scanner) After() string {
	s.start = s.cur
	s.cur = len(s.str)
	return s.emit()
}

func (s *Scanner) emit() string {
	token := s.str[s.start:s.cur]
	s.start = s.cur
	return token
}

func (s *Scanner) next() (rune, int) {
	if s.cur >= len(s.str) {
		return EOF, 0
	}
	c, n := utf8.DecodeRuneInString(s.str[s.cur:])
	return c, n
}
