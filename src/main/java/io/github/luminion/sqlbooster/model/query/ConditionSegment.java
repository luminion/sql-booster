package io.github.luminion.sqlbooster.model.query;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.NoSuchElementException;

/**
 * 条件组，用于封装一组以 AND 或 OR 连接的查询条件。
 * <p>
 * 通过 {@code next} 字段形成链表，可以构建出复杂的条件树，例如 (A AND B) AND (C OR D)。其中每个括号都是一个条件组。
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConditionSegment implements Serializable, Iterable<ConditionSegment> {
    private static final long serialVersionUID = 1L;

    /**
     * 当前节点包含的条件列表。
     */
    protected LinkedHashSet<Condition> conditions = new LinkedHashSet<>();

    /**
     * 条件列表中条件的关系是否为并且，默认true (false时为或)。
     */
    protected boolean and = true;

    /**
     * 指向下一个条件组
     */
    protected ConditionSegment next;

    @Override
    public Iterator<ConditionSegment> iterator() {
        return new Itr(this);
    }

    /**
     * 添加一组条件。
     * <p>
     * 如果连接符是 AND，条件会被添加到当前节点；如果是 OR，会创建一个新的子节点。
     *
     * @param conditions 条件集合
     * @param isAnd      条件间关系
     */
    public ConditionSegment appendConditions(Collection<Condition> conditions, boolean isAnd) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        if (isAnd) {
            this.getConditions().addAll(conditions);
        } else {
            // OR 条件，追加到链表末尾
            ConditionSegment conditionSegment = new ConditionSegment();
            conditionSegment.conditions.addAll(conditions);
            conditionSegment.and = false;
            ConditionSegment current = this;
            while (current.getNext() != null) {
                current = current.getNext();
            }
            current.next = conditionSegment;
        }
        return this;
    }

    /**
     * 合并另一个条件组。
     *
     * @param segment 要合并的条件组
     */
    public ConditionSegment merge(ConditionSegment segment) {
        if (segment == null) {
            return this;
        }
        for (ConditionSegment node : segment) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            this.appendConditions(node.getConditions(), node.isAnd());
        }
        return this;
    }

    static class Itr implements Iterator<ConditionSegment> {

        private ConditionSegment current;

        public Itr(ConditionSegment root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public ConditionSegment next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            ConditionSegment result = current;
            current = current.getNext();
            return result;
        }
    }

}
