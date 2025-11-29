package io.github.luminion.sqlbooster.model.query;

import lombok.*;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Sort implements Serializable {
    private static final long serialVersionUID = 1L;

    protected String field;

    protected boolean asc;

}
