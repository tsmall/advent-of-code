{
  concurrent-ruby = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0nwad3211p7yv9sda31jmbyw6sdafzmdi2i2niaz6f0wk5nq9h0f";
      type = "gem";
    };
    version = "1.1.9";
  };
  immutable-ruby = {
    dependencies = ["concurrent-ruby" "sorted_set"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1mbfcy85nn527inyi4qbv1cqmz2sivb6bzjrfvgi8agz6w0ns5ry";
      type = "gem";
    };
    version = "0.1.0";
  };
  rbtree = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0p0x2sxm0ar4ywsxp94yh3glawf83bdikdkccpc8zzln5987l9y1";
      type = "gem";
    };
    version = "0.4.4";
  };
  set = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "00ick64s6066idlylbxhpjmxf56h1l22c8xp0mg4j8963zga9zq2";
      type = "gem";
    };
    version = "1.0.2";
  };
  sorted_set = {
    dependencies = ["rbtree" "set"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0brpwv68d7m9qbf5js4bg8bmg4v7h4ghz312jv9cnnccdvp8nasg";
      type = "gem";
    };
    version = "1.0.3";
  };
}
