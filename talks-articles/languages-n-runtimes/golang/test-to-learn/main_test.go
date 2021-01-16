package main

import "testing"

type MockAvailableService struct {
}

type MockUnavailableService struct {
}

func (m *MockAvailableService) Info() string {
	return "mock-available-server"
}
func (m *MockAvailableService) Available() bool {
	return true
}

func (m *MockUnavailableService) Info() string {
	return "mock-unavailable-server"
}
func (m *MockUnavailableService) Available() bool {
	return false
}

func TestIsUrlAvailablePass(t *testing.T) {
	if isUrlAvailable(&MockAvailableService{}) != "available" {
		t.Errorf("isUrlAvailable failed for available service")
	}
}

func TestIsUrlAvailableFail(t *testing.T) {
	if isUrlAvailable(&MockUnavailableService{}) != "failed" {
		t.Errorf("isUrlAvailable failed for unavailable service")
	}
}
