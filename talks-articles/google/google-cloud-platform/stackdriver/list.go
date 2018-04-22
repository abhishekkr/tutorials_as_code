package main

import (
	"fmt"
	"log"

	"cloud.google.com/go/logging/logadmin"
	"golang.org/x/net/context"
	"google.golang.org/api/iterator"
	"google.golang.org/api/option"
)

var (
	GoogleProjectId = "my-project"
	GoogleLogTopic  = "cloudaudit.googleapis.com%2Factivity"
)

func main() {
	ctx := context.Background()

	adminClient, err := logadmin.NewClient(ctx,
		GoogleProjectId,
		option.WithCredentialsFile("/tmp/my-project.json"))
	if err != nil {
		log.Fatalf("Failed to create logadmin client: %v", err)
	}

	iter := adminClient.Entries(ctx,
		logadmin.Filter(fmt.Sprintf(`logName = "projects/%s/logs/%s"`,
			GoogleProjectId, GoogleLogTopic)),
		logadmin.NewestFirst(),
	)

	for {
		entry, err := iter.Next()
		if err == iterator.Done {
			log.Println("done")
		}
		if err != nil {
			log.Fatalf("Failed: %v", err)
		}

		fmt.Println(entry.Timestamp)
		fmt.Println(entry.Severity)
		fmt.Println(entry.Labels)
		fmt.Println(entry.LogName)
		fmt.Println(entry.Payload)
	}
}
