#!ruby

require 'fileutils'

module FSWork
  extend FileUtils # or FileUtils::Verbose, FileUtils::DryRun, ...

  module_function

  def do_work
    touch 'file.txt'
  end
end

FSWork.do_work
