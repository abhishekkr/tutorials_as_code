package main

import (
	"flag"
	"fmt"
	"time"

	rrd "github.com/ziutek/rrd"
)

var (
	flagAction = flag.String("action", "", "what action (db|monitor|info|fetch) do you want it to take")
)

func createDB(dbfile string, step, heartbeat uint) (err error) {
	c := rrd.NewCreator(dbfile, time.Now(), step)
	c.RRA("AVERAGE", 0.5, 1, 100)
	c.RRA("AVERAGE", 0.5, 5, 100)
	c.DS("cnt", "COUNTER", heartbeat, 0, 100)
	c.DS("g", "GAUGE", heartbeat, 0, 60)
	err = c.Create(true)
	return
}

func updateDB(dbfile string, step uint) (err error) {
	u := rrd.NewUpdater(dbfile)
	fmt.Println("Started Before:", time.Now())
	for i := 0; i < 10; i++ {
		err = u.Update(time.Now(), i, i*i)
		time.Sleep(time.Duration(step) * time.Second)
		if err != nil {
			return
		}
	}
	fmt.Println("Stopped After:", time.Now())
	return
}

func infoDB(dbfile string) (err error) {
	inf, err := rrd.Info(dbfile)
	if err != nil {
		return
	}
	for k, v := range inf {
		fmt.Printf("%s (%T): %v\n", k, v, v)
	}
	return
}

func fetchDB(dbfile string, step uint) (err error) {
	inf, err := rrd.Info(dbfile)
	if err != nil {
		return
	}

	end := time.Unix(int64(inf["last_update"].(uint)), 0)
	start := end.Add(-20 * time.Duration(step) * time.Second)
	fmt.Printf("Fetch Params:\n")
	fmt.Printf("Start: %s\n", start)
	fmt.Printf("End: %s\n", end)
	fmt.Printf("Step: %s\n", time.Duration(step)*time.Second)
	fetchRes, err := rrd.Fetch(dbfile, "AVERAGE", start, end, time.Duration(step)*time.Second)
	if err != nil {
		return
	}
	defer fetchRes.FreeValues()
	fmt.Printf("FetchResult:\n")
	fmt.Printf("Start: %s\n", fetchRes.Start)
	fmt.Printf("End: %s\n", fetchRes.End)
	fmt.Printf("Step: %s\n", fetchRes.Step)
	for _, dsName := range fetchRes.DsNames {
		fmt.Printf("\t%s", dsName)
	}
	fmt.Printf("\n")

	row := 0
	for ti := fetchRes.Start.Add(fetchRes.Step); ti.Before(end) || ti.Equal(end); ti = ti.Add(fetchRes.Step) {
		fmt.Printf("%s / %d", ti, ti.Unix())
		for i := 0; i < len(fetchRes.DsNames); i++ {
			v := fetchRes.ValueAt(i, row)
			fmt.Printf("\t%e", v)
		}
		fmt.Printf("\n")
		row++
	}
	return
}

func main() {
	flag.Parse()

	const (
		dbfile    = "/tmp/test.rrd"
		step      = 1
		heartbeat = 2 * step
	)

	var err error
	switch *flagAction {
	case "db":
		err = createDB(dbfile, step, heartbeat)
	case "monitor":
		err = updateDB(dbfile, step)
	case "info":
		err = infoDB(dbfile)
	case "fetch":
		err = fetchDB(dbfile, step)
	}
	if err != nil {
		panic(err)
	}
}
