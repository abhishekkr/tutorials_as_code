defmodule Videologue.MultimediaTest do
  use Videologue.DataCase, async: true

  alias Videologue.Multimedia

  describe "videos" do
    alias Videologue.Multimedia.Video

    @valid_attrs %{description: "some description", title: "some title", url: "some url"}
    @update_attrs %{description: "some updated description", title: "some updated title", url: "some updated url"}
    @invalid_attrs %{description: nil, title: nil, url: nil}

    setup do
      user = user_fixture(password: "i need a secret")
      category = category_fixture("testcat")
      video = video_fixture(user_id: user.id, category_id: category.id)
      {:ok, user: user, video: video, category: category}
    end

    test "list_videos/0 returns all videos", %{video: video} do
      [this_video] = Multimedia.list_videos()
      assert Videologue.Repo.preload(this_video, :user) == video
    end

    test "get_video!/1 returns the video with given id", %{video: video} do
      this_video = Multimedia.get_video!(video.id)
      assert Videologue.Repo.preload(this_video, :user) == video
    end

    test "create_video/1 with valid data creates a video" do
      assert {:ok, %Video{} = video} = Multimedia.create_video(@valid_attrs)
      assert video.description == "some description"
      assert video.title == "some title"
      assert video.url == "some url"
    end

    test "create_video/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Multimedia.create_video(@invalid_attrs)
    end

    test "update_video/2 with valid data updates the video", %{video: video} do
      assert {:ok, %Video{} = video} = Multimedia.update_video(video, @update_attrs)
      assert video.description == "some updated description"
      assert video.title == "some updated title"
      assert video.url == "some updated url"
    end

    test "update_video/2 with invalid data returns error changeset", %{video: video} do
      assert {:error, %Ecto.Changeset{}} = Multimedia.update_video(video, @invalid_attrs)

      this_video = Multimedia.get_video!(video.id)
      assert video == Videologue.Repo.preload(this_video, :user)
    end

    test "delete_video/1 deletes the video", %{video: video} do
      assert {:ok, %Video{}} = Multimedia.delete_video(video)
      assert_raise Ecto.NoResultsError, fn -> Multimedia.get_video!(video.id) end
    end

    test "change_video/1 returns a video changeset", %{video: video} do
      assert %Ecto.Changeset{} = Multimedia.change_video(video)
    end

    ## for custom func
    test "list_user_videos/1 returns all videos for a user", %{video: video, user: user} do
      [user_video] = Multimedia.list_user_videos(user)
      assert Videologue.Repo.preload(user_video, :user) == video

      user0 = user_fixture(password: "i need 0 secret")
      assert Multimedia.list_user_videos(user0) == []
    end

    test "get_user_video!/1 returns the video with given id", %{video: video, user: user} do
      this_video = Multimedia.get_user_video!(user, video.id)
      assert Videologue.Repo.preload(this_video, :user) == video
    end

    test "create_user_video/1 with valid data creates a video", %{user: user} do
      assert {:ok, %Video{} = video} = Multimedia.create_user_video(user, @valid_attrs)
      assert video.description == "some description"
      assert video.title == "some title"
      assert video.url == "some url"
    end

    test "create_user_video/1 with invalid data returns error changeset", %{user: user} do
      assert {:error, %Ecto.Changeset{}} = Multimedia.create_user_video(user, @invalid_attrs)
    end
  end

  describe "categories" do
    alias Videologue.Multimedia.Category

    @valid_attrs "what"
    @invalid_attrs nil

    test "create_category!/1 with valid data returns a Category" do
      assert {:ok, %Category{} = category} = Multimedia.create_category!(@valid_attrs)
      assert category.name == @valid_attrs
    end

    test "create_category!/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Multimedia.create_category!(@invalid_attrs)
    end

    test "list_alphabetical_categories/0 returns lexical sorted categories" do
      for c <- ~w{cats zelda action} do
        %Category{} = category_fixture(c)
      end
      categories = Multimedia.list_alphabetical_categories()
                   |> Enum.map(fn c -> c.name end)
      assert ["action", "cats", "zelda"] == categories
    end
  end
end
