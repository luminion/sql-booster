package io.github.luminion.sqlbooster.model;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Condition implements Serializable {
    private static final long serialVersionUID = 1L;

    protected String field;

    protected String operator = SqlKeyword.EQ.getSymbol();

    protected Object value;

    // @Data 的 equals/hashCode 对数组值只做引用比较，放入 LinkedHashSet 时无法去重。
    // 手动覆写后，Lombok 不再生成同名方法。
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Condition condition = (Condition) o;
        return Objects.equals(field, condition.field)
                && Objects.equals(operator, condition.operator)
                && valueEquals(value, condition.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(field, operator, valueHashCode(value));
    }

    private static boolean valueEquals(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return false;
        if (a.getClass().isArray() && b.getClass().isArray()) {
            return Arrays.deepEquals(new Object[]{a}, new Object[]{b});
        }
        return a.equals(b);
    }

    private static int valueHashCode(Object v) {
        if (v == null) return 0;
        if (v.getClass().isArray()) {
            return Arrays.deepHashCode(new Object[]{v});
        }
        return v.hashCode();
    }

}
