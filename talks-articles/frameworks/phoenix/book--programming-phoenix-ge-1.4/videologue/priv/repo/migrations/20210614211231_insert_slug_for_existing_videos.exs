defmodule Videologue.Repo.Migrations.InsertSlugForExistingVideos do
  use Ecto.Migration

  def change do
    Videologue.Multimedia.Video
    |> Videologue.Repo.all()
    |> Enum.each(fn v ->
      Videologue.Multimedia.Video.changeset(v, %{})
      |> Videologue.Repo.update!([])  end
    )
  end
end
