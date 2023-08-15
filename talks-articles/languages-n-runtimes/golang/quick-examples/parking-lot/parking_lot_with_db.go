package parkinglot

import (
	"errors"
	"fmt"
	"log"

	"github.com/google/uuid"
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
)

const DBPATH = "/tmp/sample-gorm.db"

const (
	SlotBig    = "big"
	SlotMedium = "medium"
	SlotSmall  = "small"
)

type TableParkingLot struct {
	UUID        string             `gorm:"primaryKey"`
	Created     int64              `gorm:"autoCreateTime"`
	BigSlots    []TableParkingSlot `gorm:"-:all"`
	MediumSlots []TableParkingSlot `gorm:"-:all"`
	SmallSlots  []TableParkingSlot `gorm:"-:all"`
	DB          *gorm.DB           `gorm:"-:all"`
}

type TableParkingSlot struct {
	UUID       string `gorm:"primaryKey"`
	ParkingLot string
	Car        string
	Size       string
	SlotNumber int
	Available  bool
}

func init() {
	db, err := gorm.Open(sqlite.Open(DBPATH), &gorm.Config{})
	if err != nil {
		panic(err)
	}
	// Auto Migrate
	db.AutoMigrate(&TableParkingLot{})
	db.AutoMigrate(&TableParkingSlot{})
	sqlDB, _ := db.DB()
	sqlDB.Close()
}

func DB() (*gorm.DB, error) {
	return gorm.Open(sqlite.Open(DBPATH), &gorm.Config{})
}

func (lot *TableParkingLot) CreateParkingSlots(slotSize string, count int) error {
	newSlots := make([]TableParkingSlot, count)
	for idx := 0; idx < count; idx++ {
		newSlots[idx] = TableParkingSlot{
			UUID:       uuid.New().String(),
			ParkingLot: lot.UUID,
			Size:       slotSize,
			SlotNumber: idx + 1,
			Available:  true,
		}
	}
	result := lot.DB.Create(&newSlots)
	return result.Error
}

func NewTableLot(big, medium, small int) (*TableParkingLot, error) {
	db, errDB := DB()
	if errDB != nil {
		log.Printf("FAILED to get DB handle: %v", errDB)
		return &TableParkingLot{}, errDB
	}

	lot := TableParkingLot{
		UUID: uuid.New().String(),
		DB:   db,
	}
	if result := db.Create(&lot); result.Error != nil {
		return &lot, result.Error
	}
	if err := lot.CreateParkingSlots(SlotBig, big); err != nil {
		return &lot, err
	}
	if err := lot.CreateParkingSlots(SlotMedium, medium); err != nil {
		return &lot, err
	}
	if err := lot.CreateParkingSlots(SlotSmall, small); err != nil {
		return &lot, err
	}
	return &lot, nil
}

func (lot *TableParkingLot) AddCar(carNumber, carSize string) error {
	var slot TableParkingSlot
	switch carSize {
	case SlotBig, SlotMedium, SlotSmall:
		break
	default:
		return errors.New(fmt.Sprintf("Unknown CarSize was passed: %v", carSize))
	}
	result := lot.DB.Where(
		&TableParkingSlot{ParkingLot: lot.UUID, Size: carSize, Available: true},
	).First(&slot)
	if result.Error != nil || result.RowsAffected == 0 {
		fmt.Printf("No free %s slots available, can't park %s.\n", carSize, carNumber)
		return errors.New("No free slots.")
	}

	var err error
	resultUpdate := lot.DB.Model(&slot).Updates(map[string]interface{}{
		"car":       carNumber,
		"available": false,
	})
	if resultUpdate.Error != nil || resultUpdate.RowsAffected == 0 {
		fmt.Printf("Failed to park in %s slots, car %s due to service error.\n", carSize, carNumber)
		err = resultUpdate.Error
	} else {
		fmt.Printf("Parked in %s slot %d available, parked %s.\n", carSize, slot.SlotNumber, carNumber)
	}
	return err
}

func (lot *TableParkingLot) RemoveCar(carNumber string) error {
	var slot TableParkingSlot
	result := lot.DB.Where(
		&TableParkingSlot{Car: carNumber, Available: false},
	).First(&slot)
	if result.Error != nil {
		fmt.Printf("Failed to check slots for car %s due to service error.\n", carNumber)
		return result.Error
	} else if result.RowsAffected == 0 {
		fmt.Printf("Unknown Car %s was looked for.\n", carNumber)
		return errors.New(fmt.Sprintf("Unknown Car was passed: %s", carNumber))
	}

	var err error
	resultUpdate := lot.DB.Model(&slot).Updates(map[string]interface{}{
		"car":       "",
		"available": true,
	})
	if resultUpdate.Error != nil || resultUpdate.RowsAffected == 0 {
		fmt.Printf("Failed to remove from %s slot %d, car %s due to service error.\n", slot.Size, slot.SlotNumber, carNumber)
		err = resultUpdate.Error
	} else {
		fmt.Printf("Car %s removed from %s slot #%d.\n", carNumber, slot.Size, slot.SlotNumber)
	}
	return err
}

func (lot *TableParkingLot) HasCar(carNumber string) bool {
	var slot TableParkingSlot
	result := lot.DB.Where(
		&TableParkingSlot{Car: carNumber, Available: false},
	).First(&slot)
	if result.Error != nil || result.RowsAffected == 0 {
		fmt.Printf("Unknown Car %s was looked for.\n", carNumber)
		return false
	}
	fmt.Printf("Car %s found in %s slot #%d.\n", carNumber, slot.Size, slot.SlotNumber)
	return true
}
