package io.github.luminion.sqlbooster.model.query;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.*;

/**
 * 条件组
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConditionSegment implements Serializable, Iterable<ConditionSegment> {
    private static final long serialVersionUID = 1L;
    /**
     * 当前节点的条件列表.
     */
    protected LinkedHashSet<Condition> conditions = new LinkedHashSet<>();
    /**
     * 连接当前节点条件的逻辑连接符.
     */
    protected String connector = SqlKeyword.AND.getSymbol();
    /**
     * 子条件树.
     */
    protected ConditionSegment next;

    @Override
    public Iterator<ConditionSegment> iterator() {
        return new Itr(this);
    }


    /**
     * 添加一组条件
     *
     * @param conditions 条件
     * @param connector  条件间的连接符号
     * @return this
     */
    public ConditionSegment appendConditions(Collection<Condition> conditions, String connector) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        SqlKeyword sqlKeyword = SqlKeyword.resolve(connector);
        if (SqlKeyword.OR.equals(sqlKeyword)) {
            // append or conditions to last node
            ConditionSegment node = new ConditionSegment();
            node.conditions.addAll(conditions);
            node.connector = connector;
            ConditionSegment current = this;
            while (current.getNext() != null) {
                current = current.getNext();
            }
            current.next = node;
        } else {
            // put and conditions to current level
            this.getConditions().addAll(conditions);
        }
        return this;
    }

    /**
     * 合并另一节点的条件
     *
     * @param segment 条件
     * @return this
     */
    public ConditionSegment merge(ConditionSegment segment) {
        if (segment == null) {
            return this;
        }
        for (ConditionSegment node : segment) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            this.appendConditions(node.getConditions(), node.getConnector());
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