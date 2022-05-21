(
  self: super: {
    openjdk11 = super.openjdk11.override {
      enableJavaFX = false;
    };
    openjdk15 = super.openjdk15.override {
      enableJavaFX = false;
    };
    openjdk17 = super.openjdk17.override {
      enableJavaFX = false;
    };
  }
)
