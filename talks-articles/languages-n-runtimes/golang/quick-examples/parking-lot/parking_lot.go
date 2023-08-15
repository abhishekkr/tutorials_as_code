package parkinglot

import (
	"errors"
	"fmt"
	"log"
)

type CarId string

type CarSize int

type SlotNumber int

const (
	CarBig CarSize = iota
	CarMedium
	CarSmall
)

type SlotState int

const (
	SlotAvailable SlotState = iota
	SlotBooked
)

type ParkingSystem struct {
	MaxBigSlots          int
	MaxMediumSlots       int
	MaxSmallSlots        int
	AvailableBigSlots    int
	AvailableMediumSlots int
	AvailableSmallSlots  int
	CarSlots             map[CarId]ParkingSlot
	BigSlots             map[SlotNumber]SlotState
	MediumSlots          map[SlotNumber]SlotState
	SmallSlots           map[SlotNumber]SlotState
}

type ParkingSlot struct {
	CarSize    CarSize
	SlotNumber SlotNumber
}

func New(big, medium, small int) *ParkingSystem {
	p := ParkingSystem{
		MaxBigSlots:          big,
		MaxMediumSlots:       medium,
		MaxSmallSlots:        small,
		AvailableBigSlots:    big,
		AvailableMediumSlots: medium,
		AvailableSmallSlots:  small,
		CarSlots:             map[CarId]ParkingSlot{},
		BigSlots:             map[SlotNumber]SlotState{},
		MediumSlots:          map[SlotNumber]SlotState{},
		SmallSlots:           map[SlotNumber]SlotState{},
	}
	for idx := 1; idx <= big; idx++ {
		p.BigSlots[SlotNumber(idx)] = SlotAvailable
	}
	for idx := 1; idx <= medium; idx++ {
		p.MediumSlots[SlotNumber(idx)] = SlotAvailable
	}
	for idx := 1; idx <= small; idx++ {
		p.SmallSlots[SlotNumber(idx)] = SlotAvailable
	}
	return &p
}

func (p *ParkingSystem) AddCar(carId CarId, carType CarSize) error {
	var err error
	var slotNumber = SlotNumber(-1)
	switch carType {
	case CarBig:
		slotNumber, err = p.ParkBigCar(carId)
	case CarMedium:
		slotNumber, err = p.ParkMediumCar(carId)
	case CarSmall:
		slotNumber, err = p.ParkSmallCar(carId)
	default:
		err = errors.New(fmt.Sprintf("Unknown CarSize was passed: %v", carType))
	}
	if err != nil {
		log.Printf("[ERROR] %v", err.Error())
	} else {
		log.Printf("[INFO] Parked %v at %v slot# %v", carId, carType, slotNumber)
	}
	return err
}

func (p *ParkingSystem) RemoveCar(carId CarId) error {
	parkingSlot := p.CarSlots[carId]
	switch parkingSlot.CarSize {
	case CarBig:
		if p.BigSlots[parkingSlot.SlotNumber] == SlotAvailable {
			return errors.New("Big Slot don't have this vehicle.")
		}
		delete(p.CarSlots, carId)
		p.BigSlots[parkingSlot.SlotNumber] = SlotAvailable
		p.AvailableBigSlots++
		return nil
	case CarMedium:
		if p.MediumSlots[parkingSlot.SlotNumber] == SlotAvailable {
			return errors.New("Medium Slot don't have this vehicle.")
		}
		delete(p.CarSlots, carId)
		p.MediumSlots[parkingSlot.SlotNumber] = SlotAvailable
		p.AvailableMediumSlots++
		return nil
	case CarSmall:
		if p.SmallSlots[parkingSlot.SlotNumber] == SlotAvailable {
			return errors.New("Small Slot don't have this vehicle.")
		}
		delete(p.CarSlots, carId)
		p.SmallSlots[parkingSlot.SlotNumber] = SlotAvailable
		p.AvailableSmallSlots++
		return nil
	}
	return errors.New(fmt.Sprintf("Unknown CarId was passed: %v", carId))
}

func (p *ParkingSystem) HasCar(carId CarId) bool {
	parkingSlot := p.CarSlots[carId]
	if parkingSlot.SlotNumber == SlotNumber(0) {
		fmt.Printf("%s is not parked here.\n", carId)
		return false
	}
	fmt.Printf("%s is parked at %v slot# %v\n", carId, parkingSlot.CarSize, parkingSlot.SlotNumber)
	return true
}

func (p *ParkingSystem) ParkBigCar(carId CarId) (SlotNumber, error) {
	if p.AvailableBigSlots < 1 {
		return -1, errors.New("No Big Slots available.")
	}
	for slotNumber, slotState := range p.BigSlots {
		if slotState == SlotAvailable {
			p.CarSlots[carId] = ParkingSlot{CarSize: CarBig, SlotNumber: slotNumber}
			p.BigSlots[slotNumber] = SlotBooked
			p.AvailableBigSlots--
			return slotNumber, nil
		}
	}
	return -1, errors.New(fmt.Sprintf("%d Big Slots were available, but none found during traversal.", p.AvailableBigSlots))
}

func (p *ParkingSystem) ParkMediumCar(carId CarId) (SlotNumber, error) {
	if p.AvailableMediumSlots < 1 {
		return -1, errors.New("No Medium Slots available.")
	}
	for slotNumber, slotState := range p.MediumSlots {
		if slotState == SlotAvailable {
			p.CarSlots[carId] = ParkingSlot{CarSize: CarMedium, SlotNumber: slotNumber}
			p.MediumSlots[slotNumber] = SlotBooked
			p.AvailableMediumSlots--
			return slotNumber, nil
		}
	}
	return -1, errors.New(fmt.Sprintf("%d Medium Slots were available, but none found during traversal.", p.AvailableMediumSlots))
}

func (p *ParkingSystem) ParkSmallCar(carId CarId) (SlotNumber, error) {
	if p.AvailableSmallSlots < 1 {
		return -1, errors.New("No Small Slots available.")
	}
	for slotNumber, slotState := range p.SmallSlots {
		if slotState == SlotAvailable {
			p.CarSlots[carId] = ParkingSlot{CarSize: CarSmall, SlotNumber: slotNumber}
			p.SmallSlots[slotNumber] = SlotBooked
			p.AvailableSmallSlots--
			return slotNumber, nil
		}
	}
	return -1, errors.New(fmt.Sprintf("%d Small Slots were available, but none found during traversal.", p.AvailableSmallSlots))
}
