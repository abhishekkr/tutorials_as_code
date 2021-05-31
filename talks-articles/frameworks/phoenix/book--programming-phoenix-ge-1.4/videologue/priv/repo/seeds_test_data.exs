alias Videologue.Accounts
alias Videologue.Multimedia

defmodule Seeds.Accounts.User do
  def add_test do
    %{name: "Sherlock Holmes", username: "sherlock", password: "this is sherlock"}
    |> Accounts.register_user()

    %{name: "Mycroft Holmes", username: "mycroft", password: "this is mycroft"}
    |> Accounts.register_user()

    %{name: "James Moriarty", username: "jim", password: "this is jim"}
    |> Accounts.register_user()

    %{name: "Martha Hudson", username: "hudson", password: "this is hudson"}
    |> Accounts.register_user()

    %{name: "John Watson", username: "watson", password: "this is watson"}
    |> Accounts.register_user()
  end
end

defmodule Seeds.Multimedia.Video do
  def add_test do
    sherlock = Accounts.get_user_by(username: "sherlock")
    watson = Accounts.get_user_by(username: "watson")

    video = %{title: "Keynote: Q&A on LiveView - Chris McCord | ElixirConf EU Virtual 2020", url: "https://www.youtube.com/watch?v=Rc_iqnZ05ZE", description: "At the previous ElixirConf EU Virtual conference Chris updated us on what’s new in LiveView."}
    Multimedia.create_user_video(sherlock, video)

    video = %{title: "Phoenix LiveView Uploads Deep Dive", url: "https://www.youtube.com/watch?v=PffpT2eslH8", description: "A step-by-step deep dive into the new Phoenix LiveView uploads feature."}
    Multimedia.create_user_video(sherlock, video)

    video = %{title: "Announcing Livebook - José Valim", url: "https://www.youtube.com/watch?v=RKvqc-UEe34", description: "Livebook is an open-source web application for writing interactive and collaborative code notebooks in Elixir and implemented with Phoenix LiveView."}
    Multimedia.create_user_video(sherlock, video)

    video = %{title: "Programmer Passport: Phoenix LiveView - Bonus Video - Schemaless Changesets", url: "https://www.youtube.com/watch?v=VzOyLlctkQM", description: "Supplementary for Programmer Passport book"}
    Multimedia.create_user_video(watson, video)

    video = %{title: "Introducing Nx - José Valim | Lambda Days 2021", url: "https://www.youtube.com/watch?v=fPKMmJpAGWc", description: "recorded at virtual Lambda Days conference, which took place on 16-19th February 2021"}
    Multimedia.create_user_video(watson, video)
  end
end


Seeds.Accounts.User.add_test
Seeds.Multimedia.Video.add_test
