#!ruby
#
# DUMMY, just ideation

def start
  open_listener_socket
  load_rack_app

  2.times do
    fork { worker_loop }
  end
end

def worker_loop
  loop do
    connection = @listener_socket.accept
    process_client(connection)
  end
end

def open_listener_socket; puts 'LISTENENING... (NOT)'; end
def load_rack_app; puts 'RACK APP UP... (NOT)'; end
def process_client(conn); puts "hey #{conn}; go away"; end
      class L
        attr_accessor :user
        def accept
          @user ||= 0
          @user += 1
          "user#{@user}"
        end
      end
@listener_socket = L.new
start
