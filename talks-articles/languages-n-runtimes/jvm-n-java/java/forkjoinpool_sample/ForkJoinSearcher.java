/*
 * ForkJoinPool introduced in Java7
 * similar to Executor Framework, splits task and submits smaller chunks to worker threads
 * this acts in recursive order,
 * where those smaller tasks get again split into subtasks recursively until its atomic
 * */

package forkjoinpool_sample;

import java.util.Arrays;
import java.util.concurrent.RecursiveTask;

public class ForkJoinSearcher extends RecursiveTask<Boolean>{

  int [] arr;
  int searchableElement;

  ForkJoinSearcher(int[] arr, int search){
    this.arr = arr;
    this.searchableElement = search;
  }

  @Override
  protected Boolean compute(){
    int mid=( 0 + arr.length )/2;
    System.out.println(Thread.currentThread().getName() + " says : After splliting the arry length is :: "+ arr.length + " Midpoint is " + mid);
    if (arr[mid]==searchableElement) {
      System.out.println(" FOUND !!!!!!!!!");
      return true;

    }
    else if (mid==1 || mid == arr.length) {
      System.out.println("NOT FOUND !!!!!!!!!");
      return false;

    }
    else if (searchableElement < arr[mid]) {
      System.out.println(Thread.currentThread().getName() + " says :: Doing Left-search");
      int[] left = Arrays.copyOfRange(arr, 0, mid);
      ForkJoinSearcher forkTask = new ForkJoinSearcher(left,searchableElement);
      forkTask.fork();
      return forkTask.join();

    }
    else if (searchableElement > arr[mid]) {
      System.out.println(Thread.currentThread().getName() + " says :: Doing Right-search");
      int[] right = Arrays.copyOfRange(arr, mid, arr.length);
      ForkJoinSearcher forkTask = new ForkJoinSearcher(right,searchableElement);
      forkTask.fork();
      return forkTask.join();

    }            
    return false;
  }
}
