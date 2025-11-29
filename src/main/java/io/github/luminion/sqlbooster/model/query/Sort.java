package io.github.luminion.sqlbooster.model.query;

import lombok.*;

import java.io.Serializable;

/**
 * 排序条件。
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Sort implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 需要排序的字段名。
     */
    protected String field;

    /**
     * 是否为升序排列 (true: ASC, false: DESC)。
     */
    protected boolean asc;

}
