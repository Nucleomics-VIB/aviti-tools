from services.masks_loader import (
    MASK_REGEX, is_valid_mask, parse_typed_mask, parse_uploaded_masks,
)

import pytest


def test_valid_masks():
    assert is_valid_mask("R1:Y12N*-R2:Y12N*")
    assert is_valid_mask("R1:N*-R2:N*")
    assert is_valid_mask("R1:Y10N*-R2:Y10N*")


def test_invalid_masks():
    assert not is_valid_mask("")
    assert not is_valid_mask("R1:Y12N*")
    assert not is_valid_mask("R3:Y12N*-R2:Y12N*")
    assert not is_valid_mask("R1:y12n*-R2:Y12N*")


def test_parse_typed_mask_ok():
    m = parse_typed_mask("R1:Y12N*-R2:Y12N*")
    assert m.text == "R1:Y12N*-R2:Y12N*"
    assert m.source == "typed"


def test_parse_typed_mask_bad():
    with pytest.raises(ValueError):
        parse_typed_mask("nope")


def test_parse_uploaded_yaml_list():
    content = "- R1:Y12N*-R2:Y12N*\n- R1:N*-R2:N*\n"
    out = parse_uploaded_masks(content)
    assert len(out) == 2
    assert all(m.source == "uploaded" for m in out)


def test_parse_uploaded_plaintext_with_comments():
    content = "# header\nR1:Y12N*-R2:Y12N*\n\n# blank above\nR1:N*-R2:N*\n"
    out = parse_uploaded_masks(content)
    assert [m.text for m in out] == ["R1:Y12N*-R2:Y12N*", "R1:N*-R2:N*"]


def test_parse_uploaded_rejects_bad_line():
    with pytest.raises(ValueError):
        parse_uploaded_masks("R1:Y12N*-R2:Y12N*\nbogus\n")
