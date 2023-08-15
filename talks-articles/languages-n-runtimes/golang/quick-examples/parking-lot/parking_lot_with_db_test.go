package parkinglot

import "testing"

func TestTableParkingSystem(t *testing.T) {
	_, err := NewTableLot(5, 10, 20)
	if err != nil {
		t.Error("Failed initializing ParkingSystem.")
	}
}

func TestTableParkingOp(t *testing.T) {
	p, err := NewTableLot(5, 10, 20)
	if err != nil {
		t.Error("Failed initializing ParkingSystem.")
	}

	check(t, p.AddCar("A 123", SlotBig))
	check(t, p.AddCar("A 124", SlotBig))
	check(t, p.AddCar("A 125", SlotBig))

	check(t, p.AddCar("B 235", SlotSmall))
	check(t, p.AddCar("B 245", SlotSmall))
	check(t, p.AddCar("B 255", SlotSmall))
	check(t, p.AddCar("B 265", SlotSmall))

	check(t, p.AddCar("C 355", SlotMedium))
	check(t, p.AddCar("C 365", SlotMedium))

	check(t, p.AddCar("A 126", SlotBig))
	check(t, p.AddCar("A 127", SlotBig))
	if err := p.AddCar("A 128", SlotBig); err == nil {
		t.Error("FAILED, added a car when big slots are full.")
	}

	p.HasCar("A 124")
	p.HasCar("B 124")
	p.HasCar("A 125")
	check(t, p.RemoveCar("A 125"))
	check(t, p.AddCar("A 128", SlotBig))
}

func check(t *testing.T, err error) {
	if err != nil {
		t.Error(err.Error())
	}
}
