package parkinglot

import "testing"

func TestParkingSystem(t *testing.T) {
	parkingSystem := New(5, 10, 20)
	if len(parkingSystem.BigSlots) != 5 ||
		len(parkingSystem.MediumSlots) != 10 ||
		len(parkingSystem.SmallSlots) != 20 {
		t.Error("Failed initializing ParkingSystem.")
	}
}

func TestParkingOp(t *testing.T) {
	p := New(5, 10, 20)

	p.AddCar(CarId("A 123"), CarBig)
	p.AddCar(CarId("A 124"), CarBig)
	p.AddCar(CarId("A 125"), CarBig)

	p.AddCar(CarId("B 225"), CarSmall)
	p.AddCar(CarId("B 235"), CarSmall)
	p.AddCar(CarId("B 245"), CarSmall)
	p.AddCar(CarId("B 255"), CarSmall)
	p.AddCar(CarId("B 265"), CarSmall)

	p.AddCar(CarId("C 355"), CarMedium)
	p.AddCar(CarId("C 365"), CarMedium)

	p.AddCar(CarId("A 126"), CarBig)
	p.AddCar(CarId("A 127"), CarBig)
	p.AddCar(CarId("A 128"), CarBig)

	p.HasCar(CarId("A 124"))
	p.HasCar(CarId("B 124"))
	p.HasCar(CarId("A 125"))
	p.RemoveCar(CarId("A 125"))
	p.AddCar(CarId("A 128"), CarBig)
}
