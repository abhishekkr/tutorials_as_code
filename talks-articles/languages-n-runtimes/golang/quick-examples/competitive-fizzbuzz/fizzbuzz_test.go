package main

import (
	"errors"
	"fmt"
	"testing"
)

var (
	Upto30Fizzbuzz = []string{
		"1",
		"2",
		"fizz",
		"4",
		"buzz",
		"fizz",
		"7",
		"8",
		"fizz",
		"buzz",
		"11",
		"fizz",
		"13",
		"14",
		"fizzbuzz",
		"16",
		"17",
		"fizz",
		"19",
		"buzz",
		"fizz",
		"22",
		"23",
		"fizz",
		"buzz",
		"26",
		"fizz",
		"28",
		"29",
		"fizzbuzz",
	}
)

func benchmarkFor(b *testing.B, upto int, fizzbuzzFn func(int) []string) {
	fizzbuzzCount := upto / 15
	buzzCount := fizzbuzzCount * 2
	fizzCount := buzzCount * 2
	for _, dat := range fizzbuzzFn(30) {
		if dat == "fizzbuzz" {
			fizzbuzzCount--
		} else if dat == "buzz" {
			buzzCount--
		} else if dat == "fizz" {
			fizzCount--
		}
	}
	if fizzbuzzCount != 0 || buzzCount != 0 || fizzCount != 0 {
		b.Error("FizzBuzz is broken.")
	}
}

func TestFizzBuzzModuloA(t *testing.T) {
	for idx, dat := range FizzBuzzModuloA(30) {
		if Upto30Fizzbuzz[idx] != dat {
			t.Error(errors.New("FizzBuzzModuloA is broken."))
		}
	}
}

func BenchmarkFizzBuzzModuloA(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzModuloA)
	}
}

func TestFizzBuzzModuloB(t *testing.T) {
	for idx, dat := range FizzBuzzModuloB(30) {
		if Upto30Fizzbuzz[idx] != dat {
			t.Error(errors.New("FizzBuzzModuloB is broken."))
		}
	}
}

func BenchmarkFizzBuzzModuloB(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzModuloB)
	}
}

func TestFizzBuzzModuloC(t *testing.T) {
	for idx, dat := range FizzBuzzModuloC(30) {
		if Upto30Fizzbuzz[idx] != dat {
			fmt.Println(Upto30Fizzbuzz[idx], dat)
			t.Error(errors.New("FizzBuzzModuloC is broken."))
		}
	}
}

func BenchmarkFizzBuzzModuloC(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzModuloC)
	}
}

func TestFizzBuzzModuloD(t *testing.T) {
	for idx, dat := range FizzBuzzModuloD(30) {
		if Upto30Fizzbuzz[idx] != dat {
			t.Error(errors.New("FizzBuzzModuloD is broken."))
		}
	}
}

func BenchmarkFizzBuzzModuloD(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzModuloD)
	}
}

func TestFizzBuzzModuloE(t *testing.T) {
	for idx, dat := range FizzBuzzModuloE(30) {
		if Upto30Fizzbuzz[idx] != dat {
			t.Error(errors.New("FizzBuzzModuloE is broken."))
		}
	}
}

func BenchmarkFizzBuzzModuloE(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzModuloE)
	}
}

func TestFizzBuzzIncAndMultiply(t *testing.T) {
	for idx, dat := range FizzBuzzIncAndMultiply(30) {
		if Upto30Fizzbuzz[idx] != dat {
			t.Error(errors.New("FizzBuzzIncremental is broken."))
		}
	}
}

func BenchmarkFizzBuzzIncAndMultiply(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzIncAndMultiply)
	}
}

func TestFizzBuzzIncremental(t *testing.T) {
	for idx, dat := range FizzBuzzIncremental(30) {
		if Upto30Fizzbuzz[idx] != dat {
			t.Error(errors.New("FizzBuzzIncremental is broken."))
		}
	}
}

func BenchmarkFizzBuzzIncremental(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkFor(b, 30, FizzBuzzIncremental)
	}
}
