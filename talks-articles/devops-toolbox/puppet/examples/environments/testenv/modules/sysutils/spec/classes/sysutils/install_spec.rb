require 'spec_helper'
describe 'sysutils::install' do
  it do
    should contain_package('htop').with_ensure('installed')
    should contain_package('iostat').with_ensure('installed')
    should contain_package('strace').with_ensure('installed')
  end
end
