package io.github.luminion.sqlbooster.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

// 不提供 equals/hashCode：SqlContext 是可变的链表结构，且从不作为 Set/Map 的键或参与相等比较，
// 用默认的身份比较即可，避免因 next 链是否纳入比较而反复纠结的半吊子实现。
@Getter
@Setter
@ToString(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class SqlContext<T> extends ConditionSegment {
    private static final long serialVersionUID = 1L;

    protected LinkedHashSet<Sort> sorts = new LinkedHashSet<>();

    protected Map<String, Object> params = new HashMap<>();

    @Override
    public ConditionSegment merge(ConditionSegment conditionSegment) {
        if (conditionSegment == null) {
            return this;
        }
        if (conditionSegment instanceof SqlContext) {
            SqlContext<?> sqlContext = (SqlContext<?>) conditionSegment;
            this.sorts.addAll(sqlContext.getSorts());
            this.params.putAll(sqlContext.getParams());
        }
        return super.merge(conditionSegment);
    }
}
