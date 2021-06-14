defimpl Phoenix.Param, for: Videologue.Multimedia.Video do
  def to_param(%{slug: slug, id: id}), do: "#{id}-#{slug}"
end
