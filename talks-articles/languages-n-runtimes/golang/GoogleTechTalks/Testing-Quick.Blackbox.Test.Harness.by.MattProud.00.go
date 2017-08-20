package main

import (
	"fmt"
	"math/rand"
	"reflect"
	"testing/quick"
	"unsafe"
)

var rnd = rand.New(rand.NewSource(42))

func report(typ reflect.Type) {
	defer func() {
		if err := recover(); err != nil {
			// IIRC, arrays would panic in 1.4.1 and previous.
			fmt.Println("\tCOULD NOT GENERATE")
			return
		}
	}()
	val, ok := quick.Value(typ, rnd)
	if !ok {
		fmt.Println("\tCOULD NOT GENERATE")
		return
	}
	fmt.Printf("\tGENERATED: %#v\n", val.Interface())
}

func main() {
	var table = struct {
		// Table of reflect.Kind Zero Values
		_ bool                // reflect.Bool
		_ int                 // reflect.Int
		_ int8                // reflect.Int8
		_ int16               // reflect.Int16
		_ int32               // reflect.Int32
		_ int64               // reflect.Int64
		_ uint                // reflect.Uint
		_ uint8               // reflect.Uint8
		_ uint16              // reflect.Uint16
		_ uint32              // reflect.Uint32
		_ uint64              // reflect.Uint64
		_ uintptr             // reflect.Uintptr
		_ float32             // reflect.Float32
		_ float64             // reflect.Float64
		_ complex64           // reflect.Complex64
		_ complex128          // reflect.Complex128
		_ [4]byte             // reflect.Array
		_ chan struct{}       // reflect.Chan
		_ *uint8              // reflect.Ptr
		_ func()              // reflect.Func
		_ interface{}         // reflect.Interface
		_ map[string]struct{} // reflect.Map
		_ []uint8             // reflect.Slice
		_ string              // reflect.String
		_ struct{}            // reflect.Struct
		_ unsafe.Pointer      // reflect.UnsafePointer
	}{}
	typ := reflect.TypeOf(table)
	for i := 0; i < typ.NumField(); i++ {
		fld := typ.Field(i)
		fmt.Println("type:", fld.Type, "kind:", fld.Type.Kind())
		for j := 0; j < 3; j++ {
			report(fld.Type)
		}
	}
}
