sub interesting {
    $_ = shift;
    return
         />Today is .+\'s birthday\.</
      || / like[ds]? your post in </
      || /like[ds] your new subscription\. </
      || / likes? that you're interested in </
      || /> likes? your comment: /
      || /&amp;birthdays=.*birthdays?\.<\/a>/;
}

sub boring {
    return
         / likes? your post in </
      || / likes? that you're interested in </
}
