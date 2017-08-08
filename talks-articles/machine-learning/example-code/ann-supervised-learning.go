package main

import "fmt"

type TrainingInput []int

type TrainingOutput int

type TrainingData struct {
	Input  TrainingInput
	Output TrainingOutput
}

var (
	Threshold    = 1.0
	LearningRate = 0.1
	Weights      = []float64{0.0, 0.0}

	InputZeroZero = TrainingInput([]int{0, 0})
	InputZeroOne  = TrainingInput([]int{0, 1})
	InputOneZero  = TrainingInput([]int{1, 0})
	InputOneOne   = TrainingInput([]int{1, 1})

	OutputZero = TrainingOutput(0)
	OutputOne  = TrainingOutput(1)

	TrainingDataOR = []TrainingData{
		TrainingData{InputZeroZero, OutputZero},
		TrainingData{InputZeroOne, OutputOne},
		TrainingData{InputOneZero, OutputOne},
		TrainingData{InputOneOne, OutputOne},
	}
)

func (dataUnit TrainingData) TrainOverDataUnit(errorCount int) int {
	inputs := dataUnit.Input
	expectedOutput := dataUnit.Output

	// Calculate weighted input
	weightedSum := 0.0
	for idx, inputData := range inputs {
		weightedSum += float64(inputData) * Weights[idx]
	}

	output := 0
	if Threshold <= weightedSum {
		output = 1
	}

	fmt.Printf("Target output: %d, Actual Output: %d\n", expectedOutput, output)

	errorC := float64(expectedOutput - TrainingOutput(output))

	if errorC != 0 {
		errorCount++
	}

	for idx, inputData := range inputs {
		Weights[idx] += float64(LearningRate) * errorC * float64(inputData)
	}

	return errorCount
}

func TrainOverData() (returnCode bool) {
	errorCount := 0

	fmt.Println("* train again")
	for _, dataUnit := range TrainingDataOR {
		errorCount = dataUnit.TrainOverDataUnit(errorCount)
	}

	if errorCount == 0 {
		fmt.Println("Final weights: ", Weights)
		returnCode = true
	}
	return
}

func KeepTraining() {
	for {
		success := TrainOverData()
		if success {
			break
		}
	}
}

func main() {
	fmt.Println("Perceptron")
	KeepTraining()
}
