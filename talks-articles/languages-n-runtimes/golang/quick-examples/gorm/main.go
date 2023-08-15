package main

import (
	"fmt"
	"log"

	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
)

type Product struct {
	gorm.Model
	Code  string
	Price uint
}

func main() {
	db, err := gorm.Open(sqlite.Open("/tmp/abk-test.db"), &gorm.Config{})
	if err != nil {
		panic("failed to connect database")
	}

	log.Println("migrate")
	// Migrate the schema
	db.AutoMigrate(&Product{})

	log.Println("create Product: D42 & D45")
	// Create
	db.Create(&Product{Code: "D42", Price: 100})
	db.Create(&Product{Code: "D45", Price: 150})

	log.Println("read Product: D42")
	// Read
	var product Product
	db.First(&product, 1)                 // find product with integer primary key
	db.First(&product, "code = ?", "D42") // find product with code D42

	log.Println("update Product: D42")
	// Update - update product's price to 200
	db.Model(&product).Update("Price", 200)
	// Update - update multiple fields
	db.Model(&product).Updates(Product{Price: 200, Code: "F42"}) // non-zero fields
	db.Model(&product).Updates(map[string]interface{}{"Price": 200, "Code": "F42"})

	log.Println("delete Product: D42")
	// Delete - delete product
	db.Delete(&product, 1)

	var d45 Product
	db.First(&d45, "code = ?", "D45")
	fmt.Printf("%v", d45)
}
