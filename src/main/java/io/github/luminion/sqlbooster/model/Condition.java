package io.github.luminion.sqlbooster.model;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Condition implements Serializable {
    private static final long serialVersionUID = 1L;

    protected String field;

    protected String operator = SqlKeyword.EQ.getSymbol();

    protected Object value = "";

}
