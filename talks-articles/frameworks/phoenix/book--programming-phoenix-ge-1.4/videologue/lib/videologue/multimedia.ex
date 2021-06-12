defmodule Videologue.Multimedia do
  @moduledoc """
  The Multimedia context.
  """

  import Ecto.Query, warn: false
  alias Videologue.Repo

  alias Videologue.Multimedia.Video
  alias Videologue.Multimedia.Category
  alias Videologue.Accounts.User

  @doc """
  Returns the list of videos.

  ## Examples

      iex> list_videos()
      [%Video{}, ...]

  """
  def list_videos do
    Repo.all(Video)
  end

  @doc """
  Gets a single video.

  Raises `Ecto.NoResultsError` if the Video does not exist.

  ## Examples

      iex> get_video!(123)
      %Video{}

      iex> get_video!(456)
      ** (Ecto.NoResultsError)

  """
  def get_video!(id), do: Repo.get!(Video, id)

  @doc """
  Creates a video.

  ## Examples

      iex> create_video(%{field: value})
      {:ok, %Video{}}

      iex> create_video(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_video(attrs \\ %{}) do
    %Video{}
    |> Video.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a video.

  ## Examples

      iex> update_video(video, %{field: new_value})
      {:ok, %Video{}}

      iex> update_video(video, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_video(%Video{} = video, attrs) do
    video
    |> Video.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a video.

  ## Examples

      iex> delete_video(video)
      {:ok, %Video{}}

      iex> delete_video(video)
      {:error, %Ecto.Changeset{}}

  """
  def delete_video(%Video{} = video) do
    Repo.delete(video)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking video changes.

  ## Examples

      iex> change_video(video)
      %Ecto.Changeset{data: %Video{}}

  """
  def change_video(%Video{} = video, attrs \\ %{}) do
    Video.changeset(video, attrs)
  end

  ### User specific functions

  def list_user_videos(%User{} = user) do
    Video |> user_video_query(user) |> Repo.all()
  end

  def get_user_video!(%User{} = user, id) do
    Video |> user_video_query(user) |> Repo.get!(id)
  end

  def create_user_video(%User{} = user, attrs \\ %{}) do
    %Video{}
    |> Video.changeset(attrs)
    |> Ecto.Changeset.put_assoc(:user, user)
    |> Repo.insert()
  end

  def create_category!(name) do
    %Category{name: name}
    |> Category.changeset()
    |> Repo.insert(on_conflict: :nothing)
  end

  def list_alphabetical_categories do
    Category |> Category.alphabetical() |> Repo.all()
  end

  defp user_video_query(query, %User{id: user_id}) do
    from(v in query, where: v.user_id == ^user_id)
  end
end
