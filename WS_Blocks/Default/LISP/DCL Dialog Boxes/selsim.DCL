SelSimSet : dialog {
  label = "Select Similar Setup";
  : row {
  : boxed_column {
    label = "General Properties";
    : toggle {
      label = "Entity Type (e.g. LINE, TEXT, etc.)";
      key = "0";
    }
    : toggle {
      label = "Line Type";
      key = "6";
    }
    : toggle {
      label = "Layer";
      key = "8";
    }
    : toggle {
      label = "LT Scale";
      key = "48";
    }
    : toggle {
      label = "Colour";
      key = "62";
    }
    : toggle {
      label = "Line Weight";
      key = "370";
    }
  }
  : column {
    : row {
      : boxed_column {
        label = "Text Specific Properties";
        : toggle {
          label = "Text Style";
          key = "7";
        }
        : toggle {
          label = "Text Height";
          key = "40";
        }
        : toggle {
          label = "Text Rotation";
          key = "50";
        }
      }
    }
    : row {
      : boxed_column {
        label = "Style Based Properties";
        : text { value = "such as DIM, LEADER, etc."; }
        : toggle {
          label = "Style";
          key = "3";
        }
      }
    }
    ok_cancel;
  }}
}