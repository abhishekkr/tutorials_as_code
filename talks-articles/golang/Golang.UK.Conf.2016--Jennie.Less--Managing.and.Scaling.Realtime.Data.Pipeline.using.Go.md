
## Managing and Scaling Realtime Data Pipeline using Go
> by, Jennie Less
> at Golang UK Conf 2016


### Polling Patterns


* Rate Limits

```
metricResults := make(chan *Metrics, metricRequestRateLimit)

go func() {
  /*
  no need to change your outbound channel size, esp if shared, transfer items instead
  */
  for response := range metricResults {
    metricsBuffer <- response
  }
}

for service := range metricRequests {
  /*
  limit number of concurrent requests by advantage of blocking
  */
  metricResults <- makeMetricRequest(service)
}
```


* Caching

```
type ServiceCache struct {
  Services []ServiceMetadata
}

for {
  select {
    case <- readChannel:
      /*
       return writers write via channels
      single manager goroutine makes sure data is fresh
      */
      outputChannel <- serviceCache.Services
    case newServices :=  <- updateChannel:
      serviceCache.Services = newServices
    case <- quitChannel:
      return
  }
}
```


* Caching with Mutex

```
type ServiceCache struct {
  sync.RWMutex // allows an unlimited number of readers or one writer
  Services []ServiceMetadata
}

for newServices := range updateChannel {
  serviceCache.Lock()
  serviceCache.Services = newServices
  serviceCache.Unlock()
}

func (s *ServiceCache) GetServices() []ServiceMetadata {
  s.RLock()
  defer s.RUnlock()
  return s.Services
}
```


* Ticker

```
func DiscoveryRefresher(quitChannel chan struct{},
    metadataChannel chan []ServiceMetadata) {
  /*
  time.After is true to name, no initial tick
  so an extra tck upfront
  */
  discoveryRefresh(metadataChannel)

  for {
    select {
      case <- quitChannel:
        return
      case <- time.After(refreshTime):
        discoveryRefresh(metadataChannel)
    }
  }
}
```

---

### Orchestration

* Pipeline

```
/* never put anything on channel so a minimal struct type */
func StartPipeline(quitChannel chan struct {}) {
  go DiscoveryWorker(quitChannel)
  go CollectionWorker(quitChannel)
  go TransformWorker(quitChannel)
  go PushWorker(quitChannel)
}

close(quitChannel)  // closing the channel triggers 'select' case
```


* Ordering

```
func StartPipeline() {
  go func(){
    /*
    Use a sync.WaitGroup to ensure your gorotuines have exited before proceeding
    */
    collectorWaiters.Add(1)
    defer collectorWaiters.Done()
    CollectionWorker(quitChannel)
  }()
  go PushWorker()
}

func StopePipeline() {
  /*since we can guarantee no more writes, close the buffer and use range*/
  close(quitChannel)
  collectorWaiters.Wait()
  close(metricBuffer)
}
```

---

### API Client

* Batching

```
itemBatch := make([]interface{}, 0, batchSize)
timeoutTicker := time.Tick(batchTimeout)

for {
  select {
    case item := <- buffer:
      itemBatch = append(itemBatch, item)
      sendItems = len(itemBatch) >= config.BathcSize
    case <- timeoutTicker:
      sendItems = len(itemBatch) > 0
  }
  // ... check sendItems and send if true ...
}
```

> every service exposes a finite 'batch' of metrics
> make smaller buffer of slices, not items


* Draining

> assumption: failed send == clogged channel
> receive from buffer to throw away oldest items
> want new metrics when things recover, or whatever usecase

---

## Design Goals

* interface contracts, not datastore details

* batching, draining, stateless

> 75K+ values/sec on metrics, for the test


---
