// Test file for armitage with-flake example
// Uses fmt and nlohmann_json from nixpkgs

#include <fmt/core.h>

#include <nlohmann/json.hpp>

int main() {
  nlohmann::json j = {
      {"project", "armitage"}, {"version", 1}, {"features", {"dice", "coeffects", "flakes"}}};

  fmt::print("Build system: {}\n", j.dump(2));
  return 0;
}
